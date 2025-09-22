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
            hosts = [ "localhost:${builtins.toString port}" ];
          };
          auth.type = "none";
          storage.filesystem_folder = "/var/lib/radicale/collections";
        };
      };

      environment.persistence."/persist".directories = [
        cfg.settings.storage.filesystem_folder
      ];

      services.nginx.virtualHosts."cal.sisko.wg.aciceri.dev" = {
        forceSSL = true;
        useACMEHost = "aciceri.dev";
        locations."/".proxyPass = "http://127.0.0.1:${builtins.toString port}";
        serverAliases = [ "cal.sisko.zt.aciceri.dev" ];
      };
    };
}
