{
  flake.modules.homeManager.base = {
    programs.atuin = {
      enable = true;
      settings = {
        invert = true;
        enter_accept = false;
        auto_sync = true;
        sync_frequency = "5m";
        sync_address = "https://atuin.sisko.wg.aciceri.dev";
      };
      daemon.enable = true;
      flags = [ "--disable-up-arrow" ];
    };
  };

  configurations.nixos.sisko.module =
    { config, ... }:
    {
      services.atuin = {
        enable = true;
        maxHistoryLength = 32768;
        # openRegistration = true;  # uncoment if you want to register new users
      };

      services.nginx.virtualHosts = {
        "atuin.sisko.wg.aciceri.dev" = {
          forceSSL = true;
          useACMEHost = "aciceri.dev";
          locations."/" = {
            proxyPass = "http://localhost:${builtins.toString config.services.atuin.port}";
          };
          serverAliases = [ "atuin.sisko.zt.aciceri.dev" ];
        };
      };
    };
}
