{ lib, ... }:
let
  fontsConfig =
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
          applications = 13; # keep app UIs at the same size as the terminal
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
in
{
  flake.modules.nixos.base = fontsConfig;
  flake.modules.darwin.base = fontsConfig;
}
