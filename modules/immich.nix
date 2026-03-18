{
  configurations.nixos.sisko.module =
    { config, ... }:
    {
      environment.persistence."/persist".directories = [
        config.services.immich.machine-learning.environment.MACHINE_LEARNING_CACHE_FOLDER
      ];

      services.immich = {
        enable = true;
        mediaLocation = "/tank/immich";
      };

      services.nginx.virtualHosts."photos.aciceri.dev" = {
        forceSSL = true;
        enableACME = true;
        locations."/" = {
          proxyPass = "http://localhost:${toString config.services.immich.port}";
          proxyWebsockets = true;
        };
        extraConfig = ''
          client_max_body_size 50000M;
        '';
      };
    };
}
