{
  flake.modules.nixos.base = {
    services.fail2ban = {
      enable = true;
      maxretry = 5;
      bantime = "1h";
      bantime-increment.enable = true;
    };
  };

  configurations.nixos.sisko.module =
    { config, ... }:
    let
      cfg = config.services.fail2ban;
    in
    {
      environment.persistence."/persist".directories = [ (dirOf cfg.daemonSettings.Definition.dbfile) ];

      services.fail2ban.jails = {
        nginx-botsearch.settings = {
          enabled = true;
          filter = "nginx-botsearch";
          logpath = "/var/log/nginx/access.log";
        };
        nginx-bad-request.settings = {
          enabled = true;
          filter = "nginx-bad-request";
          logpath = "/var/log/nginx/access.log";
        };
      };
    };
}
