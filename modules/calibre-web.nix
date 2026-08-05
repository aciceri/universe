{
  configurations.nixos.sisko.module =
    { config, ... }:
    let
      cfg = config.services.calibre-web;
    in
    {
      services.calibre-web = {
        enable = true;
        listen = {
          ip = "127.0.0.1";
          port = 8083;
        };
        options = {
          enableBookUploading = true;
          enableBookConversion = true;
          enableKepubify = true;
        };
      };

      environment.persistence."/persist".directories = [
        "/var/lib/${cfg.dataDir}"
      ];

      services.nginx.virtualHosts."calibre.sisko.wg.aciceri.dev" = {
        forceSSL = true;
        useACMEHost = "aciceri.dev";
        locations."/".proxyPass = "http://${cfg.listen.ip}:${toString cfg.listen.port}";
        serverAliases = [ "calibre.sisko.zt.aciceri.dev" ];
        extraConfig = ''
          allow 10.100.0.0/24;
          allow 10.100.1.0/24;
          allow 127.0.0.1;
          deny all;
        '';
      };
    };
}
