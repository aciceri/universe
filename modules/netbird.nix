{ lib, ... }:
{
  flake.modules.nixos.base =
    { config, ... }:
    let
      cfg = config.netbird;
      inherit (config.networking) hostName;
      managementHost = "netbird.geosurge.ai:443";
      managementUrl = "https://${managementHost}";
      interface = "wt0";
    in
    {
      options.netbird = {
        hosts = lib.mkOption {
          type = lib.types.listOf lib.types.str;
          default = [ "pike" ];
          description = "Hostnames enrolled into the self-hosted NetBird mesh.";
        };
      };

      config = lib.mkIf (lib.elem hostName cfg.hosts) {
        services.netbird.clients.default = {
          port = 51820;
          name = "netbird";
          inherit interface;
          hardened = true;

          environment.NB_MANAGEMENT_URL = managementUrl;

          environment.XDG_CURRENT_DESKTOP = "niri";

          config.ManagementURL = {
            Scheme = "https";
            Host = managementHost;
          };
        };

        users.users.ccr.extraGroups = [ "netbird" ];

        networking.firewall.trustedInterfaces = [ interface ];

        services.resolved.enable = true;
      };
    };
}
