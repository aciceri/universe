{ inputs, ... }:
{
  flake.modules.nixos.base =
    { pkgs, ... }:
    {
      imports = [ inputs.stylix.nixosModules.stylix ];

      stylix = {
        enable = true;
        base16Scheme = "${pkgs.base16-schemes}/share/themes/catppuccin-mocha.yaml";
        polarity = "dark";
        image = pkgs.fetchurl {
          url = "https://raw.githubusercontent.com/zhichaoh/catppuccin-wallpapers/refs/heads/main/os/nix-black-4k.png";
          hash = "sha256-HRZYeKDmfA53kb3fZxuNWvR8cE96tLrqPZhX4+z4lZA=";
        };
        cursor = {
          package = pkgs.apple-cursor;
          name = "macOS";
          size = 36;
        };
      };
    };

  flake.modules.homeManager.base =
    { config, ... }:
    {
      # TODO should be possible removing this when 26.05 is out
      gtk.gtk4.theme = config.gtk.theme;
    };
}
