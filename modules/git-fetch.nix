{ ... }:
let
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
          recursive = lib.mkEnableOption "cloning and fetching submodules";
        };
      };
    in
    {
      options.services.git-fetch.repositories = lib.mkOption {
        type = lib.types.attrsOf repositoryType;
        default = { };
      };
    };

  gitFetchServices =
    {
      config,
      pkgs,
      lib,
      ...
    }:
    let
      cfg = config.services.git-fetch;
      scripts = lib.mapAttrs (
        name: repo:
        pkgs.writeShellApplication {
          name = "git-fetch-${name}";
          runtimeInputs = with pkgs; [
            git
            git-lfs
            openssh
            coreutils
          ];
          text = ''
            repository=${lib.escapeShellArg repo.path}
            export GIT_TERMINAL_PROMPT=0
            export GIT_SSH_COMMAND="ssh -o BatchMode=yes"

            ${lib.optionalString repo.recursive ''
              init_missing_submodules() {
                local parent="$1" entry path
                if [ ! -f "$parent/.gitmodules" ]; then
                  return
                fi
                while IFS= read -r -d "" entry; do
                  path="''${entry#*$'\n'}"
                  if [ ! -e "$parent/$path/.git" ]; then
                    git -C "$parent" submodule update --init -- "$path"
                  fi
                  init_missing_submodules "$parent/$path"
                done < <(git -C "$parent" config --file .gitmodules --null --get-regexp 'submodule\..*\.path')
              }
            ''}

            if [ ! -e "$repository" ] && [ ! -L "$repository" ]; then
              mkdir -p "$(dirname "$repository")"
              git clone ${lib.optionalString repo.recursive "--recurse-submodules"} -- ${lib.escapeShellArg repo.uri} "$repository"
            else
              if [ ! -e "$repository/.git" ]; then
                echo "Not a Git checkout: $repository" >&2
                exit 1
              fi
              git -C "$repository" fetch --all --recurse-submodules=${if repo.recursive then "yes" else "no"}
            fi
            ${lib.optionalString repo.recursive ''
              # Recover partial clones without moving initialized worktrees or branches.
              init_missing_submodules "$repository"
            ''}
          '';
        }
      ) cfg.repositories;
    in
    lib.mkIf (cfg.repositories != { }) (
      lib.mkMerge [
        (lib.mkIf pkgs.stdenv.hostPlatform.isLinux {
          systemd.user.services = lib.mapAttrs' (
            name: script:
            lib.nameValuePair "git-fetch-${name}" {
              Unit.Description = "Git clone/fetch for ${name}";
              Service = {
                Type = "oneshot";
                ExecStart = lib.getExe script;
              };
            }
          ) scripts;
          systemd.user.timers = lib.mapAttrs' (
            name: repo:
            lib.nameValuePair "git-fetch-${name}" {
              Unit.Description = "Timer for git clone/fetch ${name}";
              Timer = {
                OnUnitActiveSec = "${toString repo.interval}s";
                OnStartupSec = "10s";
                Persistent = true;
              };
              Install.WantedBy = [ "timers.target" ];
            }
          ) cfg.repositories;
        })
        (lib.mkIf pkgs.stdenv.hostPlatform.isDarwin {
          launchd.agents = lib.mapAttrs' (
            name: repo:
            lib.nameValuePair "git-fetch-${name}" {
              enable = true;
              config = {
                ProgramArguments = [ (lib.getExe scripts.${name}) ];
                RunAtLoad = true;
                StartInterval = repo.interval;
                ProcessType = "Background";
                StandardOutPath = "${config.home.homeDirectory}/Library/Logs/git-fetch-${name}.log";
                StandardErrorPath = "${config.home.homeDirectory}/Library/Logs/git-fetch-${name}.err";
              };
            }
          ) cfg.repositories;
        })
      ]
    );
in
{
  flake.modules.nixos.base.home-manager.sharedModules = [
    gitFetchOptions
    gitFetchServices
  ];
  flake.modules.darwin.base.home-manager.sharedModules = [
    gitFetchOptions
    gitFetchServices
  ];
}
