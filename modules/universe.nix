{ lib, ... }:
let
  gitHosts = {
    programs.ssh.knownHosts = {
      "git.aciceri.dev".publicKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIKPBaKPx0HsJpGmMT//vo2GXvGh4ULoItq49ltCMzVw4";
      # https://api.github.com/meta
      "github.com".publicKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIOMqqnkVzrm0SdG6UOoqKLsabgH5C9okWi0dh2l9GKJl";
    };
  };
in
{
  flake.modules.nixos.base = gitHosts;
  flake.modules.darwin.base = gitHosts;

  readme.parts.intro = lib.mkAfter ''
    ## Personal workspace

    My interactive checkout lives at `~/multiverse/universe`, a submodule of
    the private `multiverse` workspace. `~/universe` and `~/geosurge` are
    symlinks to the corresponding workspace checkouts.

    Workspace management is enabled for non-root users by default and can be
    disabled with the Home Manager option `multiverse.enable = false`. Root
    does not clone private repositories, migrate checkouts or receive workspace
    aliases and flake references.

    Home Manager bootstraps the workspace over SSH and preserves previous
    checkouts as `~/universe.before-multiverse` and `~/geosurge.before-multiverse`.
    Existing independent work repositories and worktrees move into the new
    geoSurge checkout without being recloned. Migration refuses existing backup
    or repository destination collisions.
    Linked worktrees are repaired from their relocated parent repository.
    Re-running activation also repairs an interrupted migration whose symlink
    was already created.

    A systemd user timer on Linux, or launchd on macOS, fetches remote updates
    without changing populated worktrees. Missing submodules are initialized
    recursively; advancing branches and recording new submodule pins is explicit.

    On agent-enabled machines, `m` opens the multiverse root and `geo` opens
    the same root with geoSurge work context. Both accept additional OMP arguments.
    For the first NixOS switch, select the new checkout explicitly:
    `nh os switch ~/multiverse/universe`.
  '';

  flake.modules.homeManager.base =
    {
      config,
      lib,
      pkgs,
      ...
    }:
    {
      options.multiverse.enable = lib.mkOption {
        type = lib.types.bool;
        default = config.home.username != "root";
        description = "Whether to manage the user's personal workspace, repository sync and launchers.";
      };
      options.multiversePath = lib.mkOption {
        type = lib.types.path;
        default = "${config.home.homeDirectory}/multiverse";
      };
      options.universePath = lib.mkOption {
        type = lib.types.path;
        default = "${config.multiversePath}/universe";
      };
      config = lib.mkIf config.multiverse.enable {
        services.git-fetch.repositories.multiverse = {
          path = config.multiversePath;
          uri = "ssh://forgejo@git.aciceri.dev/aciceri/multiverse.git";
          recursive = true;
          interval = 1000;
        };

        home.activation.migrateWorkspaces =
          lib.hm.dag.entryBetween [ "reloadSystemd" "setupLaunchAgents" ] [ "linkGeneration" ]
            (
              let
                bootstrap =
                  if pkgs.stdenv.hostPlatform.isDarwin then
                    builtins.head config.launchd.agents.git-fetch-multiverse.config.ProgramArguments
                  else
                    builtins.head config.systemd.user.services.git-fetch-multiverse.Service.ExecStart;
              in
              ''
                if [ ! -e ${lib.escapeShellArg "${config.universePath}/.git"} ] ||
                   [ ! -e ${lib.escapeShellArg "${config.multiversePath}/geosurge/.git"} ]; then
                  run ${bootstrap}
                fi

                repair_workspace_worktrees() {
                  local legacy="$1" target="$2" backup="$1.before-multiverse"
                  local record relative destination
                  local -a worktrees=()

                  if [ ! -e "$backup/.git" ]; then
                    return
                  fi
                  # Git still records the old paths after both ends have moved.
                  # Repair only worktrees registered to this backup repository.
                  while IFS= read -r -d "" record; do
                    case "$record" in
                      "worktree $legacy/"*)
                        relative="''${record#"worktree $legacy/"}"
                        case "$relative" in */*) continue ;; esac
                        destination="$target/$relative"
                        if [ -f "$destination/.git" ]; then
                          worktrees+=("$destination")
                        fi
                        ;;
                    esac
                  done < <(${lib.getExe pkgs.git} -C "$backup" worktree list --porcelain -z)
                  run ${lib.getExe pkgs.git} -C "$backup" worktree repair "''${worktrees[@]}"
                }

                migrate_workspace() {
                  local legacy="$1" target="$2" move_repos="$3" backup="$1.before-multiverse"
                  local repo destination
                  local -a repositories=()

                  if [ "$legacy" = "$target" ]; then
                    return
                  fi
                  if [ -L "$legacy" ] && [ "$(readlink "$legacy")" = "$target" ]; then
                    # A previous activation may have linked before repair failed.
                    repair_workspace_worktrees "$legacy" "$target"
                    return
                  fi
                  if [ -z "''${DRY_RUN:-}" ] && [ ! -e "$target/.git" ]; then
                    echo "Cannot migrate $legacy: checkout is unavailable at $target" >&2
                    return 1
                  fi
                  if [ -e "$legacy" ] || [ -L "$legacy" ]; then
                    if [ -e "$backup" ] || [ -L "$backup" ]; then
                      echo "Cannot migrate $legacy: backup already exists at $backup" >&2
                      return 1
                    fi
                    if [ "$move_repos" = yes ] && [ -d "$legacy" ] && [ ! -L "$legacy" ]; then
                      for repo in "$legacy"/* "$legacy"/.[!.]*; do
                        if [ -d "$repo" ] && [ -e "$repo/.git" ]; then
                          destination="$target/''${repo##*/}"
                          if [ -e "$destination" ] || [ -L "$destination" ]; then
                            echo "Cannot migrate $repo: destination already exists at $destination" >&2
                            return 1
                          fi
                          repositories+=("$repo")
                        fi
                      done
                      for repo in "''${repositories[@]}"; do
                        run mv -- "$repo" "$target/"
                      done
                    fi
                    run mv -- "$legacy" "$backup"
                    repair_workspace_worktrees "$legacy" "$target"
                  fi
                  run ln -s -- "$target" "$legacy"
                  for repo in "''${repositories[@]}"; do
                    run ${lib.getExe pkgs.git} -C "$target/''${repo##*/}" worktree repair
                  done
                }

                migrate_workspace ${lib.escapeShellArg "${config.home.homeDirectory}/universe"} \
                  ${lib.escapeShellArg config.universePath} no
                migrate_workspace ${lib.escapeShellArg "${config.home.homeDirectory}/geosurge"} \
                  ${lib.escapeShellArg "${config.multiversePath}/geosurge"} yes
                unset -f migrate_workspace repair_workspace_worktrees
              ''
            );

        nix.registry = rec {
          universe.to = {
            type = "path";
            path = config.universePath;
          };
          u = universe;
        };
      };
    };
}
