{ config, ... }:
{
  perSystem =
    { pkgs, inputs', ... }:
    {
      packages = {
        spotube = pkgs.callPackage ./_spotube.nix { }; # TODO remove once https://github.com/NixOS/nixpkgs/pull/442180 is merged
        inherit (inputs'.nix-ai-tools.packages) claude-desktop;
      };
    };

  flake.modules.nixos.base =
    { pkgs, ... }:
    {
      nixpkgs.overlays = [
        (_: _: {
          inherit (config.allSystems.${pkgs.system}.packages)
            spotube
            claude-desktop
            ;
        })
      ];
    };
}
