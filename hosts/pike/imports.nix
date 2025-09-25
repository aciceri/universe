{ config, ... }:
{
  configurations.nixos.pike.module = {
    imports = with config.flake.modules.nixos; [ workstation ];
    home-manager.sharedModules = with config.flake.modules.homeManager; [ workstation ];
  };
}
