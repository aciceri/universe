{ inputs, ... }:
{
  flake.modules.nixos.base =
    { pkgs, ... }:
    {
      imports = [ inputs.stylix.nixosModules.stylix ];

      # stylix's kmscon target still sets services.kmscon.{extraConfig,fonts},
      # which nixpkgs-unstable removed (use services.kmscon.config now). kmscon
      # is unused here, so just disable the broken target.
      stylix.targets.kmscon.enable = false;

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

  flake.modules.darwin.base =
    { pkgs, ... }:
    {
      imports = [ inputs.stylix.darwinModules.stylix ];

      stylix = {
        enable = true;
        base16Scheme = "${pkgs.base16-schemes}/share/themes/catppuccin-mocha.yaml";
        polarity = "dark";
        image = pkgs.fetchurl {
          url = "https://raw.githubusercontent.com/zhichaoh/catppuccin-wallpapers/refs/heads/main/os/nix-black-4k.png";
          hash = "sha256-HRZYeKDmfA53kb3fZxuNWvR8cE96tLrqPZhX4+z4lZA=";
        };
      };
    };
}
