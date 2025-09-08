{ config, ... }:
{
  configurations.nixos.picard.module = {
    imports = with config.flake.modules.nixos; [
      workstation
    ];
  };
}
