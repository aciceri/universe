{ lib, ... }:
{
  flake.modules.nixos.base = {
    services.automatic-timezoned.enable = true;
    services.geoclue2.enable = true;
    services.chrony = {
      enable = lib.mkDefault true;
      servers = [
        "0.nixos.pool.ntp.org"
        "1.nixos.pool.ntp.org"
        "2.nixos.pool.ntp.org"
        "3.nixos.pool.ntp.org"
      ];
    };
  };

  configurations.nixos.sisko.module = {
    time.timeZone = lib.mkForce "Europe/Rome";
    services.geoclue2.enable = lib.mkForce false;
  };
}
