{ config, lib, ... }:
{
  # Headless single-purpose machines. `base` is shared with the workstations, so
  # this profile is mostly subtraction: everything that only makes sense in
  # front of a screen goes away, and the handful of hardening knobs srvos puts
  # in its own `server` profile take its place.
  flake.modules.nixos.server =
    { pkgs, ... }:
    {
      imports = with config.flake.modules.nixos; [ base ];

      # `base` makes nushell the default login shell. Lovely interactively,
      # but everything that drives this machine over SSH — nixos-anywhere,
      # nix-copy-closure, nixos-rebuild — sends POSIX shell fragments and
      # chokes on the first `&&`.
      users.users.root.shell = pkgs.bashInteractive;

      # Nothing prunes generations on a machine that is only ever switched
      # remotely, and /nix is what actually fills a small cloud disk.
      nix.gc = {
        automatic = true;
        dates = "weekly";
        options = "--delete-older-than 30d";
      };

      virtualisation.docker.enable = lib.mkForce false;
      services.zerotierone.enable = lib.mkForce false;
      services.udisks2.enable = lib.mkForce false;
      stylix.enable = lib.mkForce false;

      # Geolocating a machine that will never move is silly; chrony stays.
      services.automatic-timezoned.enable = lib.mkForce false;
      services.geoclue2.enable = lib.mkForce false;
      time.timeZone = lib.mkDefault "UTC";

      documentation = {
        enable = lib.mkDefault false;
        doc.enable = false;
        info.enable = false;
        man.enable = lib.mkDefault false;
        nixos.enable = lib.mkDefault false;
      };
      fonts.fontconfig.enable = lib.mkDefault false;
      programs.command-not-found.enable = false;
      environment.stub-ld.enable = lib.mkDefault false;

      # No NetworkManager without a desktop to drive it.
      networking.useNetworkd = true;
      networking.useDHCP = false;
      systemd.services.NetworkManager-wait-online.enable = false;
      systemd.network.wait-online.enable = false;
      # A config switch that restarts networkd drops the SSH session it was
      # deployed over; both daemons reload their state instead.
      systemd.services.systemd-networkd.stopIfChanged = false;
      systemd.services.systemd-resolved.stopIfChanged = false;

      services.openssh.settings = {
        X11Forwarding = false;
        KbdInteractiveAuthentication = false;
        UseDns = false;
      };

      networking.firewall = {
        enable = true;
        allowPing = true;
        logRefusedConnections = false;
      };

      boot.tmp.cleanOnBoot = true;

      # An unreachable machine that stops in emergency mode is a machine that
      # needs a console; reboot into the previous generation instead.
      systemd.enableEmergencyMode = false;
      systemd.settings.Manager = {
        RuntimeWatchdogSec = "15s";
        RebootWatchdogSec = "30s";
        KExecWatchdogSec = "1m";
      };
    };
}
