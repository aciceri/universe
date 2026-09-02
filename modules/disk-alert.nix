{
  # janeway is the one machine nobody looks at until it breaks, and the way it
  # breaks on a 40 GB disk is by filling up: a mail loop, a runaway log, or
  # /nix growing one generation at a time. Prometheus scrapes it, but the fleet
  # has no alertmanager, and this machine happens to be very good at delivering
  # mail — so it tells on itself.
  configurations.nixos.janeway.module =
    { pkgs, lib, ... }:
    let
      threshold = 80; # percent
      recipient = "andrea@ciceri.me";

      check = pkgs.writeShellApplication {
        name = "disk-usage-alert";
        runtimeInputs = [
          pkgs.coreutils
          pkgs.gawk
        ];
        text = ''
          report=$(df --output=pcent,size,used,avail,target /nix /persist / \
            | tail -n +2 \
            | awk '{ gsub("%","",$1); if ($1 + 0 >= ${toString threshold}) print }')

          [ -n "$report" ] || exit 0

          {
            echo "Subject: [janeway] disk above ${toString threshold}%"
            echo "To: ${recipient}"
            echo
            echo "USE% SIZE USED AVAIL MOUNT"
            echo "$report"
            echo
            df -h
          } | /run/wrappers/bin/sendmail -t
        '';
      };
    in
    {
      systemd.services.disk-usage-alert = {
        description = "Warn by mail when a filesystem is nearly full";
        serviceConfig = {
          Type = "oneshot";
          ExecStart = lib.getExe check;
        };
      };

      systemd.timers.disk-usage-alert = {
        wantedBy = [ "timers.target" ];
        timerConfig = {
          OnCalendar = "hourly";
          Persistent = true;
        };
      };
    };
}
