{
  flake.modules.nixos.base =
    { pkgs, ... }:
    {
      users.defaultUserShell = pkgs.nushell;
    };

  flake.modules.homeManager.base = {
    programs = {
      nushell = {
        enable = true;
        settings = {
          show_banner = false;
        };
      };

      carapace.enable = true;

      bat.enable = true;

      fzf.enable = true;

      fd.enable = true;

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
  };
}
