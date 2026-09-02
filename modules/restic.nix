# To restore something use something like
# restic-sisko restore <snapshot_id> --include /persist/var/lib/hass --target /
# To get snaphots run restic-sisko snapshots
{ lib, ... }:
let
  host = "u382036.your-storagebox.de";
  port = "23";

  # One Storage Box subaccount per machine, each jailed to its own home
  # directory, so a compromised host can only reach its own backups.
  siskoUser = "u382036-sub1";
  janewayUser = "u382036-sub3";

  storageBoxHostKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIICf9svRenC/PLKIL9nk6K/pxQgoiFC41wTNvoIncOxs";
in
{
  configurations.nixos.sisko.module =
    { config, pkgs, ... }:
    {
      secrets = {
        hetzner_storage_box_ssh_password = { };
        sisko_restic_password = { };
      };

      services.openssh.knownHosts.${host}.publicKey = storageBoxHostKey;

      services.postgresqlBackup = {
        enable = true;
        backupAll = true;
        location = "/var/backup/postgresql";
      };

      environment.persistence."/persist".directories = [
        config.services.postgresqlBackup.location
      ];

      services.restic.backups.sisko =
        let
          startStopServices = [
            "podman-*"
            "paperless-*"
            "forgejo"
            "home-assistant"
          ];
        in
        {
          paths = [
            "/persist"
            "/tank/immich"
            "/tank/paperless"
            "/tank/trilium"
            "/tank/seedvault"
            "/tank/forgejo-dumps"
            "/tank/opencloud"
          ];
          exclude = [ " /persist/var/lib/containers" ];
          passwordFile = config.age.secrets.sisko_restic_password.path;
          extraOptions = [
            "sftp.command='${lib.getExe pkgs.sshpass} -f ${config.age.secrets.hetzner_storage_box_ssh_password.path} ssh -p${port} ${siskoUser}@${host} -s sftp'"
          ];
          repository = "sftp://${siskoUser}@${host}:${port}/";
          initialize = true;
          pruneOpts = [
            "--keep-yearly 1"
            "--keep-monthly 2"
            "--keep-daily 7"
          ];
          timerConfig.OnCalendar = "daily";
          timerConfig.RandomizedDelaySec = "1h";
          backupPrepareCommand =
            startStopServices
            |> lib.concatMapStringsSep "\n" (serviceGlob: "${lib.getExe' pkgs.systemd "systemctl"} stop ${serviceGlob}");
          backupCleanupCommand =
            startStopServices
            |> lib.concatMapStringsSep "\n" (
              serviceGlob: "${lib.getExe' pkgs.systemd "systemctl"} start --no-block --all ${serviceGlob}"
            );
        };
    };

  configurations.nixos.janeway.module =
    { config, pkgs, ... }:
    {
      secrets = {
        hetzner_storage_box_janeway_password = { };
        janeway_restic_password = { };
      };

      services.openssh.knownHosts.${host}.publicKey = storageBoxHostKey;

      # Roundcube's database: its identities, contacts and preferences. The
      # mail itself lives in /var/vmail, not in postgres.
      services.postgresqlBackup = {
        enable = true;
        backupAll = true;
        location = "/var/backup/postgresql";
      };

      services.restic.backups.janeway = {
        paths = [
          "/var/vmail" # the mailboxes
          "/var/dkim" # losing these means rotating the DNS record
          "/var/lib/rspamd" # bayes and fuzzy training
          "/var/lib/redis-rspamd"
          "/var/lib/postfix" # anything still queued
          "/var/lib/dovecot"
          config.services.postgresqlBackup.location
        ];

        passwordFile = config.age.secrets.janeway_restic_password.path;
        repository = "sftp://${janewayUser}@${host}:${port}/";
        extraOptions = [
          "sftp.command='${lib.getExe pkgs.sshpass} -f ${config.age.secrets.hetzner_storage_box_janeway_password.path} ssh -p${port} ${janewayUser}@${host} -s sftp'"
        ];
        initialize = true;

        # The snapshot must contain a dump taken now, not whatever
        # postgresqlBackup's own daily timer left behind hours ago.
        backupPrepareCommand = "${lib.getExe' pkgs.systemd "systemctl"} start --wait postgresqlBackup-all.service";

        # Deliberately nothing that stops postfix or dovecot: a maildir is safe
        # to copy while it is being written to (message files are never
        # modified in place, only created and moved) and refusing SMTP for the
        # duration would only make senders retry.
        pruneOpts = [
          "--keep-daily 14"
          "--keep-weekly 8"
          "--keep-monthly 12"
        ];
        timerConfig.OnCalendar = "daily";
        timerConfig.RandomizedDelaySec = "1h";
      };
    };
}
