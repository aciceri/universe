{ config, ... }:
{
  configurations.nixos.sisko.module = {
    imports = with config.flake.modules.nixos; [
      base
      claude-code-overlay
    ];
    home-manager.sharedModules = with config.flake.modules.homeManager; [ base ];
  };
}
