{ config, ... }:
{
  flake.modules.nixos.pc = {
    imports = with config.flake.modules.nixos; [ base ];
  };

  flake.modules.homeManager.pc = {
    imports = with config.flake.modules.homeManager; [ base ];
  };
}
