{
  configurations.nixos.sisko.module =
    { config, ... }:
    let
      cfg = config.services.grafana;
      user = config.systemd.services.grafana.serviceConfig.User;
    in
    {
      secrets = {
        autistici_password = {
          mode = "770";
          group = "autistici";
        };
        grafana_admin_password.owner = user;
      };

      users.groups.autistici.members = [ user ]; # Group who has access to the autistici_password secret

      services.grafana = {
        enable = true;
        settings = {
          server = {
            domain = "status.sisko.aciceri.dev";
            http_addr = "127.0.0.1";
            http_port = 2342;
            root_url = "https://${config.services.grafana.settings.server.domain}:443/";
          };
          security = {
            admin_user = "andrea";
            admin_password = "$__file{${config.age.secrets.grafana_admin_password.path}}";
          };
          smtp = {
            enabled = true;
            host = "smtp.autistici.org:587";
            user = "andrea.ciceri@autistici.org";
            from_address = "andrea.ciceri@autistici.org";
            password = "$__file{${config.age.secrets.autistici_password.path}}";
          };
        };
      };

      environment.persistence."/persist".directories = [
        cfg.dataDir
      ];

      services.nginx.virtualHosts."status.sisko.wg.aciceri.dev" = {
        useACMEHost = "aciceri.dev";
        forceSSL = true;
        locations."/".proxyPass = "http://127.0.0.1:${builtins.toString cfg.settings.server.http_port}";
        serverAliases = [ "status.sisko.zt.aciceri.dev" ];
        extraConfig = ''
          allow 10.100.0.0/24;
          allow 10.100.1.0/24;
          allow 127.0.0.1;
          deny all;
        '';
      };
    };
}
