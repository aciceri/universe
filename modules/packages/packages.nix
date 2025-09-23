{ config, ... }:
{
  perSystem =
    { inputs', ... }:
    {
      packages = {
        inherit (inputs'.nix-ai-tools.packages) claude-desktop;
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
