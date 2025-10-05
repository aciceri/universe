{ lib, ... }:
{
  configurations.nixos.sisko.module =
    { config, pkgs, ... }:
    let
      cfg = config.services.kanidm;
      defaultGroups = [
        "forgejo.user"
      ];
      mkOAuth2 =
        {
          displayName,
          originUrl,
          originLanding,
          scopes ? [
            "openid"
            "profile"
            "email"
          ],
          groups ? [ ],
          extraConfig ? { },
        }:
        {
          inherit displayName;
          inherit originUrl;
          inherit originLanding;
          preferShortUsername = true;
          scopeMaps = lib.genAttrs groups (_group: scopes);
        }
        // extraConfig;
    in
    {
      secrets.kanidm_admin_password = {
        owner = "kanidm";
      };

      users.users.kanidm.extraGroups = [
        "acme"
        "nginx"
      ];

      environment.persistence."/persist".directories = [
        "/var/lib/kanidm"
      ];

      services.kanidm = {
        enableServer = true;
        package = pkgs.kanidmWithSecretProvisioning_1_7;

        serverSettings = {
          bindaddress = "[::]:4348";
          domain = "auth.aciceri.dev";
          origin = "https://${cfg.serverSettings.domain}";
          trust_x_forward_for = true;
          tls_key = "/var/lib/acme/${cfg.serverSettings.domain}/key.pem";
          tls_chain = "/var/lib/acme/${cfg.serverSettings.domain}/cert.pem";
          online_backup = {
            path = "/var/lib/kanidm/backups";
            schedule = "00 23 * * *";
            versions = 1;
          };
        };

        provision = {
          enable = true;

          adminPasswordFile = config.age.secrets.kanidm_admin_password.path;
          idmAdminPasswordFile = config.age.secrets.kanidm_admin_password.path;

          # To manually create a reset token run:
          # kanidm login --url https://auth.aciceri.dev --name idm_admin
          # kanidm person credential create-reset-token <person> 86400 --name idm_admin
          persons."andrea" = {
            displayName = "andrea";
            legalName = "Andrea Ciceri";
            mailAddresses = [ "andrea.ciceri@autistici.org" ];
            groups = defaultGroups ++ [ "forgejo.admin" ];
          };

          groups = {
            "forgejo.user" = { };
            "forgejo.admin" = { };
          };

          systems.oauth2 = {
            # TODO add home-assistant, immich and jellyfin
            forgejo = mkOAuth2 {
              displayName = "Forgejo";
              originUrl = "${config.services.forgejo.settings.server.ROOT_URL}/user/oauth2/kanidm/callback";
              originLanding = config.services.forgejo.settings.server.ROOT_URL;
              groups = [
                "forgejo.user"
                "forgejo.admin"
              ];
              extraConfig = {
                allowInsecureClientDisablePkce = true;
                claimMaps.groups = {
                  joinType = "array";
                  valuesByGroup = {
                    "forgejo.admin" = [ "admin" ];
                    "forgejo.user" = [ "user" ];
                  };
                };
              };
            };
          };
        };
      };

      services.nginx.virtualHosts."${cfg.serverSettings.domain}" = {
        forceSSL = true;
        enableACME = true;
        locations."/".proxyPass = "https://${cfg.serverSettings.bindaddress}";
      };
    };
}
