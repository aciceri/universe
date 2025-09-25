{ inputs, ... }:
{
  flake.modules = {
    nixos.base = {
      imports = [ inputs.nixos-facter-modules.nixosModules.facter ];
    };
  };
}
