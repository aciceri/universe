{ ... }:
{
  flake.modules.nixos.workstation = {
    services.tailscale.enable = true;
  };
}
