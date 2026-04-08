{ lib, ... }:
{
  configurations.nixos.sisko.module =
    { config, ... }:
    let
      interfaceName = "lan0";
    in
    {
      # Force a stable name for the USB ethernet dongle based on MAC address
      services.udev.extraRules = ''
        SUBSYSTEM=="net", ACTION=="add", ATTR{address}=="d0:c0:bf:2d:f2:6a", NAME="${interfaceName}"
      '';

      services.adguardhome = {
        enable = true;
        mutableSettings = true;
        settings = {
          dhcp = {
            enabled = true;
            interface_name = interfaceName;

            dhcpv4 = {
              gateway_ip = "10.1.1.1";
              range_start = "10.1.1.3";
              range_end = "10.1.1.255";
              subnet_mask = "255.255.255.0";
            };
          };
          dns = {
            upstream_dns = [
              "https://dns10.quad9.net/dns-query"
            ];

            bind_hosts = [
              "127.0.0.1"
              "10.1.1.2"
            ];
          };
        };
      };

      # otherwise it creates a directory in /var/lib/private which can't be easily persisted
      systemd.services.adguardhome.serviceConfig.DynamicUser = lib.mkForce false;

      # Disable systemd-resolved to avoid DNS loop with AdGuard Home
      services.resolved.enable = lib.mkForce false;

      networking.firewall.allowedUDPPorts = [
        53
        67
      ];
      networking.firewall.allowedTCPPorts = [ 53 ];

      # Use systemd-networkd for lan0 so the static IP is applied
      # as soon as udev creates the interface (handles hotplug properly)
      networking.useNetworkd = true;

      systemd.network.networks."10-lan0" = {
        matchConfig.Name = interfaceName;
        address = [ "10.1.1.2/24" ];
        gateway = [ "10.1.1.1" ];
        networkConfig.DHCP = "no";
      };

      environment.persistence."/persist".directories = [
        "/var/lib/AdGuardHome"
      ];

      services.nginx.virtualHosts."adguard.sisko.wg.aciceri.dev" = {
        forceSSL = true;
        useACMEHost = "aciceri.dev";
        locations."/" = {
          proxyPass = "http://localhost:${toString config.services.adguardhome.port}";
        };
        serverAliases = [ "adguard.sisko.zt.aciceri.dev" ];
        extraConfig = ''
          allow 10.100.0.0/24;
          allow 10.100.1.0/24;
          allow 127.0.0.1;
          deny all;
        '';
      };

    };
}
