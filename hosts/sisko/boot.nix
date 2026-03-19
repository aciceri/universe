{
  configurations.nixos.sisko.module =
    { pkgs, ... }:
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

        kernelParams = [
          "consoleblank=300"
        ];
      };

      # Sync the laptop panel backlight with the kernel's console blanking state.
      # The i915/eDP driver ignores DPMS via setterm, so we poll fb0/blank and
      # toggle the backlight through sysfs to actually turn the panel off/on.
      systemd.services.backlight-sync = {
        description = "Sync backlight with console blank state";
        after = [ "multi-user.target" ];
        wantedBy = [ "multi-user.target" ];
        serviceConfig = {
          Type = "simple";
          ExecStart = "${pkgs.writeShellScript "backlight-sync" ''
            BACKLIGHT=/sys/class/backlight/intel_backlight/bl_power
            FB_BLANK=/sys/class/graphics/fb0/blank
            PREV=""
            while true; do
              STATE=$(cat "$FB_BLANK")
              if [ "$STATE" != "$PREV" ]; then
                if [ "$STATE" = "1" ]; then
                  echo 1 > "$BACKLIGHT"
                else
                  echo 0 > "$BACKLIGHT"
                fi
                PREV="$STATE"
              fi
              sleep 2
            done
          ''}";
          Restart = "always";
          RestartSec = 5;
        };
      };
    };
}
