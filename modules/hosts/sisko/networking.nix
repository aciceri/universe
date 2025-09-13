{
  configurations.nixos.sisko.module = {
    networking.hostName = "sisko";
    networking.nameservers = [ "127.0.0.1" ]; # i.e. adguardhome
    services.resolved.enable = true;
  };
}
