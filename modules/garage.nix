{
  configurations.nixos.sisko.module = {config, pkgs, lib, ...}: let
      rpcPort = 3901;
      s3Port = 3900;
      garageDir = "/mnt/hd/garage";
    in {
    users.groups.garage = {};
    users.users.garage = {
      isSystemUser = true;
      group = "garage";
    };

    systemd.services.garage.serviceConfig = {
      DynamicUser = lib.mkForce false;
      User = "garage";
      Group = "garage";
    };

    systemd.tmpfiles.rules = [
      "d ${garageDir} 770 garage garage"
      "d ${garageDir}/data 770 garage garage"
      "d ${garageDir}/meta 770 garage garage"
    ];

    secrets.garage_environment.owner = "garage";

    services.garage = {
      enable = true;
      package = pkgs.garage_2;
      environmentFile = config.age.secrets.garage_environment.path;
      settings = {
        replication_factor = 1;
        consistency_mode = "consistent";
        rpc_bind_addr = "0.0.0.0:${toString rpcPort}";
        rpc_public_addr = "0.0.0.0:${toString rpcPort}";
        s3_api = {
          api_bind_addr = "0.0.0.0:${toString s3Port}";
          s3_region = "garage";
        };
        data_dir = [{
          capacity = "200G";
          path = "${garageDir}/data";
        }];
        metadata_dir = "${garageDir}/meta";
      };
    };
    
    services.nginx.virtualHosts = {
      "cache.sisko.wg.aciceri.dev" = {
        forceSSL = true;        
        useACMEHost = "aciceri.dev";
        locations."/".proxyPass = "http://localhost:${builtins.toString s3Port}";
        extraConfig = ''
          proxy_max_temp_file_size 0;
        '';
      };
    };
  };
}
