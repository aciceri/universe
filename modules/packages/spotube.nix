# TODO remove once https://github.com/NixOS/nixpkgs/pull/442180 is merged
{ config, ... }:
{
  perSystem =
    { pkgs, ... }:
    {
      packages.spotube = pkgs.callPackage ./_spotube.nix { };
    };

  flake.modules.nixos.pc =
    { pkgs, ... }:
    {
      nixpkgs.overlays = [
        (_: _: {
          inherit (config.flake.packages.${pkgs.system}) spotube;
        })
      ];
    };
}
