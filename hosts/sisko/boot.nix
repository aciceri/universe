{
  configurations.nixos.sisko.module =
    { pkgs, lib, ... }:
    let
      timeout = lib.getExe' pkgs.coreutils "timeout";
      dd = lib.getExe' pkgs.coreutils "dd";
    in
    {
      boot = {
        loader = {
          systemd-boot.enable = true;
          efi.canTouchEfiVariables = true;
        };

        initrd.availableKernelModules = [
          "nvme"
          "xhci_pci"
          "usb_storage"
          "thunderbolt"
        ];
      };

      # Turn off the laptop panel backlight after 5 minutes of keyboard
      # inactivity and turn it back on at the next keypress.  This replaces
      # the kernel's consoleblank + DPMS path which does not wake reliably
      # on i915/eDP panels.
      systemd.services.backlight-idle = {
        description = "Turn off backlight after keyboard inactivity";
        after = [ "multi-user.target" ];
        wantedBy = [ "multi-user.target" ];
        serviceConfig = {
          Type = "simple";
          Restart = "always";
          RestartSec = 5;
          ExecStart = pkgs.writeShellScript "backlight-idle" ''
            BACKLIGHT=/sys/class/backlight/intel_backlight/bl_power
            KBD=/dev/input/by-path/platform-i8042-serio-0-event-kbd
            IDLE=300

            while true; do
              # Read one input event (24 bytes on x86_64) with a timeout.
              # If we keep receiving events, reset the timer each time.
              while ${timeout} "$IDLE" ${dd} bs=24 count=1 if="$KBD" of=/dev/null 2>/dev/null; do
                echo 0 > "$BACKLIGHT"
              done

              # Timeout expired - turn off the backlight
              echo 1 > "$BACKLIGHT"

              # Block until the next keypress, then turn it back on
              ${dd} bs=24 count=1 if="$KBD" of=/dev/null 2>/dev/null
              echo 0 > "$BACKLIGHT"
            done
          '';
        };
      };
    };
}
