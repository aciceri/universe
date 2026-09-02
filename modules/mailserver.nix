{ inputs, ... }:
{
  # The reason janeway exists. simple-nixos-mailserver wires up postfix,
  # dovecot, rspamd and opendkim; everything domain specific lives here, the
  # DNS side of it lives in infra/dns.tf.
  configurations.nixos.janeway.module =
    { config, ... }:
    let
      domain = "ciceri.me";
      fqdn = "mail.${domain}";
    in
    {
      imports = [ inputs.nixos-mailserver.nixosModules.default ];

      secrets = {
        mailserver_hashed_password_andrea = { };
        mailserver_hashed_password_leonardo = { };
      };

      security.acme = {
        acceptTerms = true;
        defaults.email = "andrea@${domain}";
      };

      # postfix and dovecot need a certificate for the fqdn, and HTTP-01 is the
      # cheapest way to get one. The virtual host must terminate TLS with that
      # same certificate: without `forceSSL` nginx falls back to the default
      # server for https://mail.ciceri.me and answers with Roundcube's
      # certificate, which is exactly the "certificate belongs to a different
      # site" a mail client complains about while probing for autoconfig.
      networking.firewall.allowedTCPPorts = [
        80
        443
      ];
      services.nginx = {
        enable = true;
        virtualHosts.${fqdn} = {
          enableACME = true;
          forceSSL = true;
        };
      };

      mailserver = {
        enable = true;
        stateVersion = 5;
        inherit fqdn;
        domains = [ domain ];

        x509.useACMEHost = fqdn;

        accounts."andrea@${domain}" = {
          hashedPasswordFile = config.age.secrets.mailserver_hashed_password_andrea.path;
          # Catches every address of the domain that is not an account of its
          # own, and may send as those too, which `catchAll` alone would not
          # allow. Postfix resolves the full address before the wildcard, so
          # adding an account below takes its own mail out of here.
          aliases = [ "@${domain}" ];
        };

        accounts."leonardo@${domain}".hashedPasswordFile = config.age.secrets.mailserver_hashed_password_leonardo.path;

        # ClamAV alone would eat most of the 4 GB this machine has.
        virusScanning = false;
      };
    };
}
