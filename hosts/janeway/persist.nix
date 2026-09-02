{ inputs, ... }:
{
  configurations.nixos.janeway.module = {
    imports = [ inputs.impermanence.nixosModules.impermanence ];

    # The root filesystem is a tmpfs (see disko.nix), so this list is the whole
    # truth about what state this machine has. Anything not mentioned here is
    # gone at the next reboot, which is the point.
    environment.persistence."/persist" = {
      hideMounts = true;

      directories = [
        # The mail itself, and the DKIM key pair whose public half is published
        # in DNS — losing it means rotating the record.
        "/var/vmail"
        "/var/dkim"

        # Spam filtering: bayes, fuzzy hashes and the redis backing them.
        "/var/lib/rspamd"
        "/var/lib/redis-rspamd"

        # Anything still queued, plus dovecot's indexes and roundcube's state.
        "/var/lib/postfix"
        "/var/lib/dovecot"
        "/var/lib/roundcube"
        "/var/lib/postgresql"
        "/var/backup/postgresql"

        # Certificates: re-issuing on every reboot would hit Let's Encrypt
        # rate limits soon enough.
        "/var/lib/acme"

        # Ban state, so a reboot is not an amnesty.
        "/var/lib/fail2ban"

        # uid/gid allocations; without this, ownership under /var/vmail drifts.
        "/var/lib/nixos"
        "/var/lib/systemd"
        "/var/log"
      ];

      files = [
        "/etc/machine-id"
        "/etc/ssh/ssh_host_ed25519_key"
        "/etc/ssh/ssh_host_ed25519_key.pub"
      ];
    };

    # agenix runs before the bind mounts exist, so it needs the real path.
    age.identityPaths = [ "/persist/etc/ssh/ssh_host_ed25519_key" ];

    fileSystems."/persist".neededForBoot = true;
  };
}
