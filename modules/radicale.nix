{
  configurations.nixos.sisko.module =
    { config, ... }:
    let
      cfg = config.services.radicale;
      port = 5232;
    in
    {
      services.radicale = {
        enable = true;
        settings = {
          server = {
            hosts = [ "localhost:${toString port}" ];
          };
          auth.type = "none";
          storage.filesystem_folder = "/var/lib/radicale/collections";
        };
      };

      environment.persistence."/persist".directories = [
        cfg.settings.storage.filesystem_folder
      ];

      services.nginx.virtualHosts = {
        "cal.sisko.wg.aciceri.dev" = {
          forceSSL = true;
          useACMEHost = "aciceri.dev";
          locations."/".proxyPass = "http://127.0.0.1:${toString port}";
          serverAliases = [ "cal.sisko.zt.aciceri.dev" ];
          extraConfig = ''
            allow 10.100.0.0/24;
            allow 10.100.1.0/24;
            allow 127.0.0.1;
            deny all;
          '';
        };
        # Put the web interface behind WireGuard but leave the rest public to make the calendars shareable
        "cal.aciceri.dev" = {
          enableACME = true;
          forceSSL = true;
          locations = {
            "/".proxyPass = "http://127.0.0.1:${toString port}";
            "/.web/".extraConfig = "return 404;";
          };
        };
      };
    };
}
