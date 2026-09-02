# Janeway is a Hetzner Cloud cx23 in Nuremberg, created by the OpenTofu project
# in infra/. It exists to run the mail server for ciceri.me, so it stays as
# close to the bare `server` profile as it can.
{ config, ... }:
{
  configurations.nixos.janeway.module = {
    imports = with config.flake.modules.nixos; [
      server
      hetzner-cloud
    ];
  };
}
