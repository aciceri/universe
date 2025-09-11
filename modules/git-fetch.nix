{ config, lib, ... }:
{
  flake.modules.homeManager.git-fetch =
    { config, pkgs, ... }:
    let
      cfg = config.services.git-fetch;

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

      mkGitCloneService = name: repo: {
        Unit = {
          Description = "Git fetch for ${name}";
        };
        Service = {
          Type = "oneshot";
          ExecStart = "${
            pkgs.writeShellApplication {
              name = "git-fetch-${name}";
              runtimeInputs = [
                pkgs.git
                pkgs.openssh
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
            }
          }/bin/git-fetch-${name}";
        };
      };

      mkGitCloneTimer = name: repo: {
        Unit = {
          Description = "Timer for git clone/fetch ${name}";
        };
        Timer = {
          OnUnitActiveSec = "${toString repo.interval}s";
          OnBootSec = "10s";
          Persistent = true;
        };
        Install = {
          WantedBy = [ "timers.target" ];
        };
      };
    in
    {
      options = {
        services.git-fetch = {
          repositories = lib.mkOption {
            type = lib.types.attrsOf repositoryType;
            default = { };
          };
        };
      };

      config = lib.mkIf (cfg.repositories != { }) {
        systemd.user.services =
          cfg.repositories
          |> lib.mapAttrs (name: repo: mkGitCloneService name repo)
          |> lib.mapAttrs' (name: service: lib.nameValuePair "git-fetch-${name}" service);
        systemd.user.timers =
          cfg.repositories
          |> lib.mapAttrs (name: repo: mkGitCloneTimer name repo)
          |> lib.mapAttrs' (name: timer: lib.nameValuePair "git-fetch-${name}" timer);
      };
    };

  flake.modules.nixos.base = {
    home-manager.sharedModules = [ config.flake.modules.homeManager.git-fetch ];
  };
}
