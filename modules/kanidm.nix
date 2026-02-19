{ lib, ... }:
{
  configurations.nixos.sisko.module =
    { config, pkgs, ... }:
    let
      cfg = config.services.kanidm;
      defaultGroups = [
        "forgejo.user"
        "immich.user"
        "opencloud.user"
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
        server.enable = true;
        package = pkgs.kanidmWithSecretProvisioning_1_8;

        server.settings = {
          bindaddress = "[::]:4348";
          domain = "auth.aciceri.dev";
          origin = "https://${cfg.server.settings.domain}";
          tls_key = "/var/lib/acme/${cfg.server.settings.domain}/key.pem";
          tls_chain = "/var/lib/acme/${cfg.server.settings.domain}/cert.pem";
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
            "opencloud.user" = { };
            "opencloud.admin" = { };
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
            opencloud = {
              public = true;
              displayName = "OpenCloud";
              originUrl = "https://cloud.aciceri.dev/oidc-callback.html";
              originLanding = "https://cloud.aciceri.dev";
              preferShortUsername = true;
              scopeMaps = lib.genAttrs [ "opencloud.user" "opencloud.admin" "god" ] (_: [
                "openid"
                "profile"
                "email"
              ]);
              claimMaps.roles = {
                joinType = "array";
                valuesByGroup = {
                  "opencloud.admin" = [ "opencloudAdmin" ];
                  "opencloud.user" = [ "opencloudUser" ];
                  "god" = [ "opencloudAdmin" ];
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

      services.nginx.virtualHosts."${cfg.server.settings.domain}" = {
        forceSSL = true;
        enableACME = true;
        locations."/".proxyPass = "https://${cfg.server.settings.bindaddress}";
      };
    };
}
