{
  configurations.nixos.picard.module = {
    networking = {
      hostName = "picard";
      hostId = "5b02e763";

      interfaces.enp11s0.wakeOnLan = {
        enable = true;
        policy = [ "magic" ];
      };
    };
  };
}
