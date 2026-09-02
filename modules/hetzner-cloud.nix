{ lib, ... }:
{
  # Hardware profile for the x86 Hetzner Cloud line (cx/cpx), modelled on
  # srvos' `hardware-hetzner-cloud`. Deliberately without cloud-init: the
  # machine is installed by nixos-anywhere, which already places the SSH keys,
  # and everything else here is static.
  flake.modules.nixos.hetzner-cloud =
    { modulesPath, pkgs, ... }:
    {
      imports = [ "${modulesPath}/profiles/qemu-guest.nix" ];

      # SeaBIOS, not UEFI: the x86 instances boot GRUB off the MBR gap of the
      # disk. Only the Ampere (cax) line is UEFI.
      # `devices` is a default because disko fills it in from whichever disk
      # carries the BIOS boot partition, and two definitions would collide.
      boot.loader.grub = {
        enable = true;
        efiSupport = false;
        devices = lib.mkDefault [ "/dev/sda" ];
      };

      # The Hetzner web console is a serial terminal.
      boot.kernelParams = [
        "console=ttyS0,115200"
        "console=tty0"
      ];

      # Powers the "reset root password" button in the Hetzner console, which
      # is the only way back in when SSH is broken. It needs `chpasswd`.
      services.qemuGuest.enable = true;
      systemd.services.qemu-guest-agent.path = [ pkgs.shadow ];

      # IPv4 comes from DHCP. IPv6 does not: Hetzner routes a /64 to the
      # instance and expects a static address plus a default route through the
      # link-local address of the router, which is not on any configured
      # subnet, hence GatewayOnLink.
      networking.useNetworkd = true;
      systemd.network.networks."10-uplink" = {
        matchConfig.Name = "en*";
        networkConfig = {
          DHCP = "ipv4";
          IPv6AcceptRA = false;
        };
        routes = [
          {
            Gateway = "fe80::1";
            GatewayOnLink = true;
          }
        ];
      };

      # `facter` covers the rest of the virtio hardware.
      nixpkgs.hostPlatform = lib.mkDefault "x86_64-linux";
    };
}
