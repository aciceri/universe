{ config, ... }:
{
  perSystem =
    { pkgs, inputs', ... }:
    {
      packages = {
        inherit (inputs'.nix-ai-tools.packages) claude-desktop;
        mirror-checks = pkgs.callPackage ./mirror-checks/_package.nix { };
      };
    };

  flake.modules.nixos.base =
    { pkgs, ... }:
    {
      nixpkgs.overlays = [
        (_: _: {
          inherit (config.allSystems.${pkgs.system}.packages)
            claude-desktop
            ;
        })
      ];
    };
}
