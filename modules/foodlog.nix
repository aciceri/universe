{
  configurations.nixos.sisko.module =
    { config, ... }:
    let
      cfg = config.services.foodlog;
    in
    {
      environment.persistence."/persist".directories = [
        cfg.backend.dataDir
      ];

      services.foodlog = {
        backend = {
          enable = true;
          model = "opus";
          corsOrigins = [ "https://${cfg.frontend.virtualHost}" ];
          # Authentication: run `sudo -u foodlog HOME=/var/lib/foodlog claude login`
          # to store Claude credentials in /var/lib/foodlog/.claude/
        };
        frontend.virtualHost = "food.sisko.wg.aciceri.dev";
      };

      services.nginx.virtualHosts."${cfg.frontend.virtualHost}" = {
        forceSSL = true;
        useACMEHost = "aciceri.dev";
        extraConfig = ''
          allow 10.100.0.0/24;
          allow 10.100.1.0/24;
          allow 127.0.0.1;
          deny all;
        '';
      };
    };
}
