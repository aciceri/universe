{
  configurations.nixos.sisko.module =
    { config, ... }:
    let
      cfg = config.services.amule;
    in
    {
      secrets.amule_password.owner = cfg.user;

      services.amule = {
        enable = true;
        openPeerPorts = true;
        openWebServerPort = true;
        ExternalConnectPasswordFile = config.age.secrets.amule_password.path;
        WebServerPasswordFile = config.age.secrets.amule_password.path;
        settings = {
          eMule = {
            IncomingDir = "/tank/amule";
            TempDir = "/tank/amule/Temp";
          };
          WebServer = {
            Enabled = 1;
          };
        };
      };

      environment.persistence."/persist".directories = [
        cfg.dataDir
      ];

      services.nginx.virtualHosts."amule.sisko.wg.aciceri.dev" = {
        forceSSL = true;
        useACMEHost = "aciceri.dev";
        locations."/" = {
          proxyPass = "http://localhost:${toString cfg.settings.WebServer.Port}";
        };
        serverAliases = [ "amule.sisko.zt.aciceri.dev" ];
        extraConfig = ''
          allow 10.100.0.0/24;
          allow 10.100.1.0/24;
          allow 127.0.0.1;
          deny all;
        '';
      };
    };

  flake.modules.homeManager.workstation =
    { pkgs, ... }:
    {
      home.packages = [ pkgs.amule-gui ];
    };
}
