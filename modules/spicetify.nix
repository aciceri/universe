{ inputs, ... }:
{
  flake.modules.darwin.spicetify =
    { pkgs, ... }:
    let
      spicePkgs = inputs.spicetify.legacyPackages.${pkgs.stdenv.system};
    in
    {
      imports = [ inputs.spicetify.darwinModules.spicetify ];

      # Installs a spiced Spotify (don't also install pkgs.spotify / a cask).
      programs.spicetify = {
        enable = true;
        enabledExtensions = with spicePkgs.extensions; [
          adblockify
        ];
      };
    };
}
