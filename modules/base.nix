{ config, ... }:
{
  flake.modules.nixos.base.imports = with config.flake.modules.nixos; [
    ssh
    agenix
  ];
}
