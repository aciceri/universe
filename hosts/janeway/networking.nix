{
  configurations.nixos.janeway.module = {
    networking.hostName = "janeway";

    # Hetzner routes 2a01:4f8:1c16:8198::/64 to this instance and hands out
    # nothing over DHCPv6 or RA. ::1 is the address published as the AAAA of
    # mail.ciceri.me and the one whose PTR the OpenTofu project sets, so it is
    # pinned here rather than derived by SLAAC. The default route through
    # fe80::1 comes from the hetzner-cloud module.
    systemd.network.networks."10-uplink".address = [ "2a01:4f8:1c16:8198::1/64" ];

    # Part of the WireGuard mesh so it can pull from ncps and ship logs to
    # sisko; no NetworkManager on a headless box, so the tunnel is a plain
    # kernel interface and it is the one the firewall trusts.
    wireguard = {
      useNetworkManager = false;
      interfaceName = "wg-universe";
    };
  };
}
