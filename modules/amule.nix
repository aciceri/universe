{
  configurations.nixos.sisko.module =
    { config, pkgs, ... }:
    let
      cfg = config.services.amule;
    in
    {
      services.amule = {
        enable = true;
        openPeerPorts = true;
        openWebServerPort = true;
        # TODO the service is accessible only from the VPN, using agenix would be better
        ExternalConnectPasswordFile = pkgs.writeText "password" "password";
        WebServerPasswordFile = pkgs.writeText "password" "password";
        settings = {
          eMule = {
            IncomingDir = "/mnt/hd/amule";
            TempDir = "/mnt/hd/amule/Temp";
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
