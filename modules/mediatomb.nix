{
  configurations.nixos.sisko.module = {
    services.mediatomb = {
      enable = true;
      openFirewall = true;
      serverName = "Sisko";
      interface = "lan0";
      mediaDirectories = [
        {
          path = "/tank/media/movies";
          recursive = true;
        }
        {
          path = "/tank/media/series";
          recursive = true;
        }
      ];
    };

    users.users.mediatomb.extraGroups = [
      "radarr"
      "sonarr"
    ];
  };
}
