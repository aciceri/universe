{
  configurations.nixos.sisko.module = {
    networking.hostName = "sisko";
    networking.hostId = "25973e99";
    # networking.nameservers = [ "127.0.0.1" ]; # i.e. adguardhome
    networking.nameservers = [ "10.1.1.1" ]; # TODO revert to the line above (change router settings also)
    services.resolved.enable = true;
  };
}
