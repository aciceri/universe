{ config, ... }:
{
  configurations.nixos.sisko.module = {
    imports = with config.flake.modules.nixos; [ base ];
    home-manager.sharedModules = with config.flake.modules.homeManager; [ base ];
  };
}
