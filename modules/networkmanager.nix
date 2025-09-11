{ lib, ... }:
{
  flake.modules.nixos.pc = {
    networking.networkmanager.enable = true;
    networking.useDHCP = lib.mkForce true;
  };
}
