{
  configurations.nixos.sisko.module =
    { config, pkgs, ... }:
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
        grafana_secret_key.owner = user;
      };

      users.groups.autistici.members = [ user ]; # Group who has access to the autistici_password secret
      users.groups.foodlog.members = [ user ]; # Allow Grafana to read foodlog SQLite database

      services.grafana = {
        enable = true;
        declarativePlugins = with pkgs.grafanaPlugins; [
          frser-sqlite-datasource
        ];
        provision.datasources.settings.datasources = [
          {
            name = "Foodlog";
            type = "frser-sqlite-datasource";
            jsonData = {
              path = "/var/lib/foodlog/foodlog.db";
            };
            readOnly = true;
          }
        ];
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
            secret_key = "$__file{${config.age.secrets.grafana_secret_key.path}}";
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
        locations."/".proxyPass = "http://127.0.0.1:${toString cfg.settings.server.http_port}";
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
