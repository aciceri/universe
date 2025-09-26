{ lib, ... }:
{
  flake.modules.homeManager.base = lib.mkMerge [
    (
      { config, pkgs, ... }:
      {
        programs.git = {
          enable = true;
          lfs.enable = true;
          extraConfig = {
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
              pager = "${pkgs.delta}/bin/delta --dark --paging=never";
            };
          };
        };
      }
    )
    (
      { config, ... }:
      lib.mkIf (config.home.username == "ccr") {
        programs.git = {
          userName = "Andrea Ciceri";
          userEmail = "andrea.ciceri@autistici.org";
          extraConfig = {
            github.user = "aciceri";
          };
        };
      }
    )
  ];
}
