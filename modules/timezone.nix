{ lib, ... }:
{
  flake.modules.nixos.base = {
    services.automatic-timezoned.enable = true;
    services.geoclue2.enable = true;
    services.timesyncd.enable = lib.mkDefault true;
  };

  configurations.nixos.sisko.module = {
    services.automatic-timezoned.enable = lib.mkForce false;
    services.geoclue2.enable = lib.mkForce false;
    services.timesyncd.enable = lib.mkForce false;
  };
}
