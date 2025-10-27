{
  configurations.nixos.sisko.module =
    { config, pkgs, ... }:
    let
      rev = "966199fe1dccc9c6c7016bdb1d9582f27797bc02";
      amule-flake = builtins.getFlake "github:NixOS/nixpkgs/${rev}";
      inherit (amule-flake.legacyPackages.${pkgs.system}) amule-daemon amule-web;
      cfg = config.services.amule;
    in
    {
      disabledModules = [ "services/networking/amuled.nix" ];

      imports = [ "${amule-flake}/nixos/modules/services/networking/amuled.nix" ];

      documentation.nixos.checkRedirects = false; # TODO remove when the amule PR will be merged upstream

      services.amule = {
        enable = true;
        package = amule-daemon;
        amuleWebPackage = amule-web;
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
          proxyPass = "http://localhost:${builtins.toString cfg.settings.WebServer.Port}";
        };
        serverAliases = [ "amule.sisko.zt.aciceri.dev" ];
      };
    };

  flake.modules.homeManager.workstation =
    { pkgs, ... }:
    {
      home.packages = [ pkgs.amule-gui ];
    };
}
