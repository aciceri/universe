{ lib, ... }:
{
  flake.modules.homeManager.base = lib.mkMerge [
    (
      { config, pkgs, ... }:
      {
        programs.git = {
          enable = true;
          lfs.enable = true;
          settings = {
            ui.color = "auto";
            pull.rebase = true;
            rebase.autostash = true;

            user.signingKey = "${config.home.homeDirectory}/.ssh/id_ed25519";
            gpg.format = "ssh";
            commit.gpgsign = true;

            core = {
              fsmonitor = true;
              untrackedcache = true;
            };
            fetch.writeCommitGraph = true;
          };
        };

        programs.lazygit = {
          enable = true;
          settings = {
            git.paging = {
              colorArg = "always";
              useConfig = false;
              pager = "${lib.getExe pkgs.delta} --dark --paging=never";
            };
          };
        };

        programs.nushell.shellAliases = {
          lg = lib.getExe config.programs.lazygit.package;
        };
      }
    )
    (
      { config, ... }:
      lib.mkIf (config.home.username == "ccr") {
        programs.git.settings = {
          user = {
            name = "Andrea Ciceri";
            email = "andrea.ciceri@autistici.org";
          };
          github.user = "aciceri";
        };
      }
    )
  ];
}
