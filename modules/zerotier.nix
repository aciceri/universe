{
  flake.modules.nixos.base = {
    services.zerotierone = {
      enable = true;
      joinNetworks = [ "632ea29085af0cb4" ];
    };
  };

  configurations.nixos.sisko.module = {
    environment.persistence."/persist".directories = [
      "/var/lib/zerotier-one"
    ];
  };
}
