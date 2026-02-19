{
  configurations.nixos.sisko.module =
    { config, ... }:
    let
      cfg = config.services.opencloud;
      kanidmDomain = config.services.kanidm.server.settings.domain;
    in
    {
      environment.persistence."/persist".directories = [
        cfg.stateDir
      ];

      services.opencloud = {
        enable = true;
        url = "https://cloud.aciceri.dev";

        # Custom CSP policy: extend defaults to allow Kanidm OIDC
        # This file is deep-merged with the built-in CSP defaults
        settings.csp.directives = {
          # SES/lockdown (used by OpenCloud web) requires eval
          script-src = [ "'unsafe-eval'" ];
          # Token exchange and userinfo endpoints are on Kanidm's domain
          connect-src = [ "https://${kanidmDomain}" ];
        };

        environment = {
          # TLS is terminated by nginx
          PROXY_TLS = "false";
          # Default port 9100 conflicts with node_exporter (Prometheus)
          WEB_HTTP_ADDR = "127.0.0.1:9106";

          # Disable built-in IDP - we use Kanidm instead
          # Keep IDM (LDAP) running for user auto-provisioning
          OC_EXCLUDE_RUN_SERVICES = "idp";

          # Use Kanidm as external OIDC provider
          OC_OIDC_ISSUER = "https://${kanidmDomain}/oauth2/openid/opencloud";
          OC_OIDC_CLIENT_ID = "opencloud";
          PROXY_OIDC_REWRITE_WELLKNOWN = "true";
          PROXY_OIDC_ACCESS_TOKEN_VERIFY_METHOD = "none";

          # Point the SPA to the rewritten well-known on the same origin
          # so it doesn't need to cross-origin fetch from Kanidm directly
          WEB_OIDC_METADATA_URL = "https://cloud.aciceri.dev/.well-known/openid-configuration";

          # CSP extension for Kanidm OIDC (deep-merged with defaults)
          PROXY_CSP_CONFIG_FILE_LOCATION = "/etc/opencloud/csp.yaml";

          # User provisioning and mapping
          PROXY_AUTOPROVISION_ACCOUNTS = "true";
          PROXY_USER_OIDC_CLAIM = "preferred_username";
          PROXY_USER_CS3_CLAIM = "username";
          GRAPH_USERNAME_MATCH = "none";

          # Role assignment via OIDC claims from Kanidm
          PROXY_ROLE_ASSIGNMENT_DRIVER = "oidc";
          PROXY_ROLE_ASSIGNMENT_OIDC_CLAIM = "roles";

          # Store user files on the HDD instead of the default stateDir
          STORAGE_USERS_POSIX_ROOT = "/mnt/hd/opencloud";
        };
      };

      # Allow OpenCloud to write to the HDD storage path
      # (the service runs with ProtectSystem=strict)
      systemd.services.opencloud.serviceConfig.ReadWritePaths = [ "/mnt/hd/opencloud" ];

      services.nginx.virtualHosts."cloud.aciceri.dev" = {
        forceSSL = true;
        enableACME = true;
        locations."/" = {
          proxyPass = "http://${cfg.address}:${toString cfg.port}";
          proxyWebsockets = true;
        };
        extraConfig = ''
          client_max_body_size 50000M;
        '';
      };
    };
}
