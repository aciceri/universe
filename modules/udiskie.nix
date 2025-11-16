{
  flake.modules.homeManager.base = {
    services.udiskie.enable = true;
  };

  flake.modules.nixos.base = {
    services.udisks2.enable = true;
  };
}
