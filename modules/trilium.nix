{
  configurations.nixos.sisko.module =
    { config, ... }:
    {
      services.trilium-server = {
        enable = true;
        dataDir = "/mnt/hd/trilium";
        nginx = {
          hostName = "trilium.sisko.wg.aciceri.dev";
          enable = true;
        };
      };

      systemd.tmpfiles.rules = [
        "d ${config.services.trilium-server.dataDir} 770 trilium trilium"
      ];

      services.nginx.virtualHosts."trilium.sisko.wg.aciceri.dev" = {
        forceSSL = true;
        useACMEHost = "aciceri.dev";
        serverAliases = [ "trilium.sisko.zt.aciceri.dev" ];
        extraConfig = ''
          allow 10.100.0.0/24;
          allow 10.100.1.0/24;
          allow 127.0.0.1;
          deny all;
        '';
      };
    };
}
