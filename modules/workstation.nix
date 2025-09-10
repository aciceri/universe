{ config, ... }:
{
  flake.modules.nixos.workstation = {
    imports = with config.flake.modules.nixos; [ pc ];
  };

  flake.modules.homeManager.workstation = {
    imports = with config.flake.modules.homeManager; [ pc ];
  };
}
