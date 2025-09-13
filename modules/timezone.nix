{ lib, ... }:
{
  flake.modules.nixos.base = {
    services.automatic-timezoned.enable = true;
    services.geoclue2.enable = true;
    services.timesyncd.enable = lib.mkDefault true;
  };

  configurations.nixos.sisko.module = {
    time.timeZone = lib.mkForce "Europe/Rome";
    services.geoclue2.enable = lib.mkForce false;
  };
}
