{
  flake.modules.homeManager.base =
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
          package = pkgs.noto-fonts-emoji;
          name = "Noto Color Emoji";
        };
      };
    };
}
