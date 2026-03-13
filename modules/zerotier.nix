{
  flake.modules.nixos.base = {
    services.zerotierone = {
      enable = true;
      joinNetworks = [
        "632ea29085af0cb4" # universe
        "096e27c0f271be6b" # addictive clan
      ];
    };
  };

  configurations.nixos.sisko.module = {
    environment.persistence."/persist".directories = [
      "/var/lib/zerotier-one"
    ];
  };
}
