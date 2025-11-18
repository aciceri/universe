{ lib, ... }:
{
  configurations.nixos.sisko.module =
    { config, pkgs, ... }:
    let
      cfg = config.services.kanidm;
      defaultGroups = [
        "forgejo.user"
        "immich.user"
      ];
    in
    {
      secrets.kanidm_admin_password.owner = "kanidm";

      users.users.kanidm.extraGroups = [
        "acme"
        "nginx"
      ];

      environment.persistence."/persist".directories = [
        "/var/lib/kanidm"
      ];

      services.kanidm = {
        enableServer = true;
        package = pkgs.kanidmWithSecretProvisioning_1_8;

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
          persons = {
            andrea = {
              displayName = "andrea";
              legalName = "Andrea Ciceri";
              mailAddresses = [ "andrea.ciceri@autistici.org" ];
              groups = defaultGroups ++ [ "god" ];
            };
            mara = {
              displayName = "mara";
              mailAddresses = [ "mara.savastano@gmail.com" ];
              groups = defaultGroups;
            };
          };

          groups = {
            "god" = { }; # utility group used to easily make myself admin everywhere
            "forgejo.user" = { };
            "forgejo.admin" = { };
            "immich.user" = { };
            "immich.admin" = { };
          };

          # TODO would it make sense for each oauth2 app to live in the same file defining the service?
          # To retrieve the client secrets run
          # kanidm system oauth2 show-basic-secret <appName> --url https://auth.aciceri.dev
          systems.oauth2 = {
            # TODO add home-assistant and jellyfin
            forgejo = {
              displayName = "Forgejo";
              originUrl = "${config.services.forgejo.settings.server.ROOT_URL}/user/oauth2/kanidm/callback";
              originLanding = config.services.forgejo.settings.server.ROOT_URL;
              preferShortUsername = true;
              scopeMaps = lib.genAttrs [ "forgejo.user" "forgejo.admin" "god" ] (_: [
                "openid"
                "profile"
                "email"
              ]);
              allowInsecureClientDisablePkce = true;
              claimMaps.groups = {
                joinType = "array";
                valuesByGroup = {
                  "forgejo.admin" = [ "admin" ];
                  "forgejo.user" = [ "user" ];
                  "god" = [ "admin" ];
                };
              };
            };
            immich = {
              displayName = "Immich";
              originUrl = [
                "https://photos.aciceri.dev/auth/login"
                "https://photos.aciceri.dev/api/oauth/mobile-redirect"
              ];
              originLanding = "https://photos.aciceri.dev";
              allowInsecureClientDisablePkce = true;
              preferShortUsername = true;
              scopeMaps = lib.genAttrs [ "immich.user" "immich.admin" "god" ] (_: [
                "openid"
                "profile"
                "email"
              ]);
              claimMaps.groups = {
                joinType = "array";
                valuesByGroup = {
                  "immich.admin" = [ "admin" ];
                  "immich.user" = [ "user" ];
                  "god" = [ "admin" ];
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
