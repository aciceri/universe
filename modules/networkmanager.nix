{ lib, ... }:
{
  flake.modules.nixos.pc = {
    networking.networkmanager.enable = true;
    networking.useDHCP = lib.mkForce true;

    users.users.ccr.extraGroups = [ "networkmanager" ];
  };
}
