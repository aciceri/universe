{ config, lib, ... }:
{
  configurations.nixos.sisko.module =
    { pkgs, ... }:
    let
      # VM network (libvirt default NAT)

      vmIp = "192.168.122.10";
      vmBridge = "virbr0";

      # Host physical interface (same as used in wireguard.nix)
      hostInterface = "enP4p65s0";

      # SSH port exposed on host for reaching the VM
      hostSshPort = 2222;
    in
    {
      virtualisation.libvirtd = {
        enable = true;
        qemu.vhostUserPackages = [ pkgs.virtiofsd ];
      };

      # Give god users access to libvirt and KVM
      users.users =
        config.users
        |> lib.mapAttrs (
          _: user: {
            extraGroups = lib.optionals user.god [
              "libvirtd"
              "kvm"
            ];
          }
        );

      # Port forward: host:2222 -> VM:22 (SSH)
      # Using manual DNAT rules instead of networking.nat.forwardPorts because
      # the latter binds to externalInterface only, excluding WireGuard access.
      networking.firewall.allowedTCPPorts = [ hostSshPort ];

      # DNAT and LAN isolation rules that don't depend on libvirt chains
      networking.firewall.extraCommands = ''
        # Port forward host:${toString hostSshPort} -> VM:22 (any interface)
        iptables -t nat -A PREROUTING -p tcp --dport ${toString hostSshPort} -j DNAT --to-destination ${vmIp}:22
        iptables -t nat -A POSTROUTING -d ${vmIp} -p tcp --dport 22 -j MASQUERADE

        # Allow established/related connections back to VM
        iptables -A FORWARD -i ${vmBridge} -m state --state ESTABLISHED,RELATED -j ACCEPT

        # Block VM -> private subnets (LAN isolation)
        iptables -A FORWARD -i ${vmBridge} -d 10.0.0.0/8 -j DROP
        iptables -A FORWARD -i ${vmBridge} -d 172.16.0.0/12 -j DROP
        iptables -A FORWARD -i ${vmBridge} -d 192.168.0.0/16 -j DROP

        # Allow VM -> internet via physical interface
        iptables -A FORWARD -i ${vmBridge} -o ${hostInterface} -j ACCEPT
      '';

      networking.firewall.extraStopCommands = ''
        iptables -t nat -D PREROUTING -p tcp --dport ${toString hostSshPort} -j DNAT --to-destination ${vmIp}:22 || true
        iptables -t nat -D POSTROUTING -d ${vmIp} -p tcp --dport 22 -j MASQUERADE || true
        iptables -D FORWARD -i ${vmBridge} -m state --state ESTABLISHED,RELATED -j ACCEPT || true
        iptables -D FORWARD -i ${vmBridge} -d 10.0.0.0/8 -j DROP || true
        iptables -D FORWARD -i ${vmBridge} -d 172.16.0.0/12 -j DROP || true
        iptables -D FORWARD -i ${vmBridge} -d 192.168.0.0/16 -j DROP || true
        iptables -D FORWARD -i ${vmBridge} -o ${hostInterface} -j ACCEPT || true
      '';

      # Insert SSH ACCEPT into libvirt's LIBVIRT_FWI chain after libvirtd
      # creates it (extraCommands runs before libvirtd, so we need a
      # separate service that runs after)
      systemd.services.clan-test-fw = {
        description = "Allow SSH forwarding to clan-test VM through libvirt firewall";
        after = [
          "libvirtd.service"
          "libvirt-guests.service"
        ];
        requires = [ "libvirtd.service" ];
        wantedBy = [ "multi-user.target" ];
        # libvirtd creates LIBVIRT_FWI only when it starts a network, which
        # may happen after libvirtd.service is "ready". Retry until the chain
        # exists.
        serviceConfig = {
          Type = "oneshot";
          RemainAfterExit = true;
          Restart = "on-failure";
          RestartSec = 3;
          ExecStart = "${pkgs.iptables}/bin/iptables -I LIBVIRT_FWI 1 -p tcp -d ${vmIp} --dport 22 -j ACCEPT";
          ExecStop = "${pkgs.iptables}/bin/iptables -D LIBVIRT_FWI -p tcp -d ${vmIp} --dport 22 -j ACCEPT";
        };
      };

      # VM disk storage directory on the HDD
      systemd.tmpfiles.rules = [
        "d /mnt/hd/libvirt 0711 root root -"
        "d /mnt/hd/libvirt/images 0711 root root -"
      ];

      # Persist libvirt state across reboots (impermanence)
      environment.persistence."/persist".directories = [
        "/var/lib/libvirt"
        "/etc/libvirt"
      ];
    };
}
