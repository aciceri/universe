{ ... }:
let
  # Option declaration (cross-platform).
  gitFetchOptions =
    { lib, ... }:
    let
      repositoryType = lib.types.submodule {
        options = {
          path = lib.mkOption {
            type = lib.types.str;
          };
          uri = lib.mkOption {
            type = lib.types.str;
          };
          interval = lib.mkOption {
            type = lib.types.int;
          };
        };
      };
    in
    {
      options.services.git-fetch.repositories = lib.mkOption {
        type = lib.types.attrsOf repositoryType;
        default = { };
      };
    };

  # systemd-user implementation (Linux only).
  gitFetchSystemd =
    {
      config,
      pkgs,
      lib,
      ...
    }:
    let
      cfg = config.services.git-fetch;

      mkGitCloneService = name: repo: {
        Unit.Description = "Git fetch for ${name}";
        Service = {
          Type = "oneshot";
          ExecStart =
            (pkgs.writeShellApplication {
              name = "git-fetch-${name}";
              runtimeInputs = with pkgs; [
                git
                openssh
                coreutils
              ];
              text = ''
                if [ ! -d "${repo.path}" ]; then
                  mkdir -p "$(dirname "${repo.path}")"
                  git clone "${repo.uri}" "${repo.path}"
                else
                  cd "${repo.path}"
                  git fetch --all
                fi
              '';
            })
            |> lib.getExe;
        };
      };

      mkGitCloneTimer = name: repo: {
        Unit.Description = "Timer for git clone/fetch ${name}";
        Timer = {
          OnUnitActiveSec = "${toString repo.interval}s";
          OnStartupSec = "10s";
          Persistent = true;
        };
        Install.WantedBy = [ "timers.target" ];
      };
    in
    lib.mkIf (cfg.repositories != { }) {
      systemd.user.services =
        cfg.repositories
        |> lib.mapAttrs (name: repo: mkGitCloneService name repo)
        |> lib.mapAttrs' (name: service: lib.nameValuePair "git-fetch-${name}" service);
      systemd.user.timers =
        cfg.repositories
        |> lib.mapAttrs (name: repo: mkGitCloneTimer name repo)
        |> lib.mapAttrs' (name: timer: lib.nameValuePair "git-fetch-${name}" timer);
    };
in
{
  # Inject HM modules at system level.
  flake.modules.nixos.base = {
    home-manager.sharedModules = [
      gitFetchOptions
      gitFetchSystemd
    ];
  };

  # No git-fetch implementation for darwin (launchd) yet, only the options.
  flake.modules.darwin.base = {
    home-manager.sharedModules = [ gitFetchOptions ];
  };
}
