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

  configurations.nixos.janeway.module = {
    # Roundcube authenticates against dovecot over the *public* address, so a
    # brute force aimed at the webmail is logged by dovecot as coming from
    # janeway itself. Banning that address would take the whole mail server
    # off the internet; the roundcube-auth jail below sees the real client.
    services.fail2ban.ignoreIP = [
      "2.28.76.183"
      "2a01:4f8:1c16:8198::/64"
      "10.100.0.0/24"
    ];

    services.fail2ban.jails = {
      dovecot.settings = {
        enabled = true;
        backend = "systemd";
        journalmatch = "_SYSTEMD_UNIT=dovecot.service";
        # dovecot 2.4 reworded its login failures and the filter shipped with
        # fail2ban 1.1 still expects the 2.3 wording, so the pattern is spelled
        # out here. It matches what dovecot.conf's own `prefregex` leaves
        # behind, i.e. the message with `<host> dovecot[pid]: imap-login: `
        # already stripped — not the raw journal line.
        failregex = ''^Login aborted: .*\(auth failed, [0-9]+ attempts.*rip=<HOST>,'';
      };

      postfix-sasl.settings = {
        enabled = true;
        backend = "systemd";
        filter = "postfix[mode=auth]";
      };

      roundcube-auth.settings = {
        enabled = true;
        backend = "systemd";
        filter = "roundcube-auth";
      };
    };
  };
}
