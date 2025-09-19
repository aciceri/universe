{
  flake.modules.nixos.base =
    { pkgs, ... }:
    {
      users.defaultUserShell = pkgs.nushell;
      programs.mosh.enable = true;
    };

  flake.modules.homeManager.base =
    { pkgs, ... }:
    {
      programs = {
        nushell = {
          enable = true;
          settings = {
            show_banner = false;
          };
        };

        carapace.enable = true;

        bat = {
          enable = true;
          extraPackages = with pkgs.bat-extras; [
            batdiff
            batman
            batgrep
            batwatch
          ];
        };

        fzf.enable = true;

        fd.enable = true;

        ripgrep.enable = true;

        ripgrep-all.enable = true;

        broot.enable = true;

        direnv = {
          enable = true;
          config = {
            warn_timeout = "60s";
          };
          nix-direnv.enable = true;
        };

        pay-respects.enable = true;

        television.enable = true;

        ranger = {
          enable = true;
          settings = {
            "preview_images" = true;
            "preview_images_method" = "sixel";
          };
        };

        zoxide.enable = true;

        btop = {
          enable = true;
          settings.update_ms = 100;
        };
      };

      home.packages = with pkgs; [
        repgrep
        tokei
      ];
    };
}
