{
  # Webmail for the ciceri.me mailbox. It talks to dovecot/postfix over the
  # public names rather than localhost so the TLS certificate actually matches
  # and the same credentials work from here and from a desktop client.
  configurations.nixos.janeway.module =
    { config, ... }:
    let
      inherit (config.mailserver) fqdn;
    in
    {
      services.roundcube = {
        enable = true;

        # Deliberately NOT mailserver.fqdn, even though serving both from one
        # virtual host works: that name is the one in the MX, the PTR and the
        # EHLO, and its certificate is the one postfix and dovecot present. A
        # web virtual host owning that certificate means a failed HTTP-01
        # renewal takes TLS away from SMTP and IMAP too.
        hostName = "webmail.ciceri.me";

        # The module only generates the database, plugin and key settings; the
        # mail server to talk to is up to us.
        extraConfig = ''
          $config['imap_host'] = "ssl://${fqdn}:993";
          $config['smtp_host'] = "ssl://${fqdn}:465";
          $config['smtp_user'] = "%u";
          $config['smtp_pass'] = "%p";
          $config['product_name'] = "ciceri.me";
        '';
      };

      # The roundcube module turns on forceSSL + enableACME for its virtual
      # host; 80 is already open for the ACME challenge.
      networking.firewall.allowedTCPPorts = [ 443 ];
    };
}
