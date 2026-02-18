{
  flake.modules.nixos.base =
    { pkgs, ... }:
    {
      stylix.fonts = {
        serif = {
          package = pkgs.dejavu_fonts;
          name = "DejaVu Serif";
        };

        sansSerif = {
          package = pkgs.dejavu_fonts;
          name = "DejaVu Sans";
        };

        monospace = {
          package = pkgs.iosevka-comfy.comfy;
          name = "Iosevka Comfy";
        };

        emoji = {
          package = pkgs.noto-fonts-color-emoji;
          name = "Noto Color Emoji";
        };

        sizes = {
          terminal = 13;
          desktop = 13;
        };
      };
      fonts.packages =
        with pkgs;
        [
          powerline-fonts
          dejavu_fonts
          et-book
          vegur
        ]
        ++ (builtins.filter lib.attrsets.isDerivation (builtins.attrValues nerd-fonts));

    };
}
