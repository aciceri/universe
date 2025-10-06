{ lib, ... }:
{
  configurations.nixos.sisko.module = {
    services.radarr = {
      enable = true;
    };

    services.sonarr = {
      enable = true;
    };

    services.prowlarr = {
      enable = true;
    };

    systemd.services.prowlarr = {
      serviceConfig = {
        DynamicUser = lib.mkForce false;
      };
    };

    services.lidarr = {
      enable = true;
    };

    users.users.radarr.extraGroups = [ "transmission" ];
    users.users.sonarr.extraGroups = [ "transmission" ];
    users.users.lidarr.extraGroups = [ "transmission" ];

    environment.persistence."/persist".directories = [
      "/var/lib/radarr"
      "/var/lib/prowlarr"
      "/var/lib/sonarr"
      "/var/lib/lidarr"
    ];

    services.nginx.virtualHosts = {
      "radarr.sisko.wg.aciceri.dev" = {
        forceSSL = true;
        useACMEHost = "aciceri.dev";
        locations."/" = {
          proxyPass = "http://localhost:7878"; # FIXME hardcoded port
        };
        serverAliases = [ "radarr.sisko.zt.aciceri.dev" ];
      };
      "prowlarr.sisko.wg.aciceri.dev" = {
        forceSSL = true;
        useACMEHost = "aciceri.dev";
        locations."/" = {
          proxyPass = "http://localhost:9696"; # FIXME hardcoded port
        };
        serverAliases = [ "prowlarr.sisko.zt.aciceri.dev" ];
      };
      "sonarr.sisko.wg.aciceri.dev" = {
        forceSSL = true;
        useACMEHost = "aciceri.dev";
        locations."/" = {
          proxyPass = "http://localhost:8989"; # FIXME hardcoded port
        };
        serverAliases = [ "sonarr.sisko.zt.aciceri.dev" ];
      };
      "lidarr.sisko.wg.aciceri.dev" = {
        forceSSL = true;
        useACMEHost = "aciceri.dev";
        locations."/" = {
          proxyPass = "http://localhost:8686"; # FIXME hardcoded port
        };
        serverAliases = [ "lidarr.sisko.zt.aciceri.dev" ];
      };
    };
  };
}
