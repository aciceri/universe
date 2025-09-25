{ config, ... }:
{
  configurations.nixos.picard.module = {
    imports = with config.flake.modules.nixos; [ workstation ];
    home-manager.sharedModules = with config.flake.modules.homeManager; [ workstation ];
  };
}
