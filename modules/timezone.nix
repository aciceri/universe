{
  flake.modules.nixos.base = {
    services.automatic-timezoned.enable = true;
    services.geoclue2.enable = true;
    services.timesyncd.enable = true;
  };
}
