# To restore something use something like
# restic-sisko restore <snapshot_id> --include /persist/var/lib/hass --target /
# To get snaphots run restic-sisko snapshots
{ lib, ... }:
let
  user = "u382036-sub1";
  host = "u382036.your-storagebox.de";
  port = "23";
in
{
  configurations.nixos.sisko.module =
    { config, pkgs, ... }:
    {
      secrets = {
        hetzner_storage_box_ssh_password = { };
        sisko_restic_password = { };
      };

      services.openssh.knownHosts."${host}".publicKey =
        "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIICf9svRenC/PLKIL9nk6K/pxQgoiFC41wTNvoIncOxs";

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
            "/mnt/hd/immich"
            "/mnt/hd/paperless"
            "/mnt/hd/roam"
            "/mnt/hd/trilium"
            "/mnt/hd/siyuan"
            "/mnt/hd/seedvault"
            "/mnt/hd/forgejo-dumps"
          ];
          exclude = [ " /persist/var/lib/containers" ];
          passwordFile = config.age.secrets.sisko_restic_password.path;
          extraOptions = [
            "sftp.command='${lib.getExe pkgs.sshpass} -f ${config.age.secrets.hetzner_storage_box_ssh_password.path} ssh -p${port} ${user}@${host} -s sftp'"
          ];
          repository = "sftp://${user}@${host}:${port}/";
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
}
