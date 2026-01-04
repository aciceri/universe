{
  configurations.nixos.sisko.module =
    { config, ... }:
    let
      cfg = config.services.foodlog;
    in
    {
      secrets.foodlog_environment_file.owner = "foodlog";

      environment.persistence."/persist".directories = [
        cfg.backend.dataDir
      ];

      services.foodlog = {
        backend = {
          enable = true;
          corsOrigins = [ "https://${cfg.frontend.virtualHost}" ];
          environmentFile = config.age.secrets.foodlog_environment_file.path;
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
