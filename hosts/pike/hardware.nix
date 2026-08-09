{
  configurations.nixos.pike.module =
    { config, pkgs, ... }:
    let
      healthMode = "/sys/bus/wmi/drivers/acer-wmi-battery/health_mode";
    in
    {
      facter.reportPath = ./facter.json;

      powerManagement = {
        cpuFreqGovernor = "schedutil";

        # The USB-C hub brings its DisplayPort link back up before the DDC
        # channel is readable: right after resume niri probes DP-1, gets an
        # empty EDID and falls back to 1024x768, leaving the external
        # monitor black until the next hotplug (which can take minutes).
        # Re-probe any connected DP connector that still has no EDID so the
        # kernel emits a fresh hotplug event with the real modes.
        resumeCommands = ''
          for attempt in 1 2 3; do
            sleep 3
            settled=1
            for conn in /sys/class/drm/card*-DP-*; do
              status="$(cat "$conn/status" || true)"
              [ "$status" = connected ] || continue
              if [ ! -s "$conn/edid" ]; then
                settled=0
                echo detect > "$conn/status" || true
              fi
            done
            if [ "$settled" = 1 ]; then break; fi
          done
        '';
      };

      # Battery care: Acer "health mode" makes the firmware cap charging at
      # 80%, which slows down cell wear on a mostly-plugged-in laptop.
      # `sudo battery-full` lifts the cap (e.g. the night before a trip);
      # the cap is restored automatically after 12h or when AC is unplugged.
      boot = {
        extraModulePackages = [ config.boot.kernelPackages.acer-wmi-battery ];
        kernelModules = [ "acer_wmi_battery" ];
        extraModprobeConfig = "options acer_wmi_battery enable_health_mode=1";
      };

      systemd.services.battery-care = {
        description = "Re-enable Acer battery health mode (80% charge cap)";
        serviceConfig.Type = "oneshot";
        script = ''
          if [ -w ${healthMode} ]; then
            echo 1 > ${healthMode}
          fi
        '';
      };

      systemd.services.battery-full = {
        description = "Temporarily allow charging to 100%";
        serviceConfig.Type = "oneshot";
        script = ''
          echo 0 > ${healthMode}
          systemctl restart battery-care.timer
        '';
      };

      # Not in any wantedBy: started only by battery-full as a 12h fallback.
      systemd.timers.battery-care = {
        description = "Restore the 80% charge cap 12h after battery-full";
        timerConfig.OnActiveSec = "12h";
      };

      environment.systemPackages = [
        (pkgs.writeShellScriptBin "battery-full" "exec systemctl start battery-full.service")
        (pkgs.writeShellScriptBin "battery-care" "exec systemctl start battery-care.service")
      ];

      hardware = {
        cpu.intel.updateMicrocode = true;
        enableRedistributableFirmware = true;
        graphics = {
          enable = true;
          enable32Bit = true;
        };

        nvidia = {
          open = true;
          prime = {
            offload = {
              enable = true;
              enableOffloadCmd = true;
            };
            intelBusId = "PCI:0:2:0";
            nvidiaBusId = "PCI:1:0:0";
          };
        };
        nvidia-container-toolkit.enable = true;
      };

      zramSwap = {
        enable = true;
        algorithm = "zstd";
      };

      services = {
        zfs.autoScrub.enable = true;
        # Loads the nvidia kernel driver (works under Wayland too); required by
        # nvidia-container-toolkit and hardware.nvidia.prime.
        xserver.videoDrivers = [ "nvidia" ];
        power-profiles-daemon.enable = true;
        upower.enable = true;
        scx = {
          enable = true;
          scheduler = "scx_bpfland";
        };

        # Restore the cap as soon as the charger is unplugged: by then the
        # full charge served its purpose, and the cap only bites on replug.
        udev.extraRules = ''
          SUBSYSTEM=="power_supply", KERNEL=="ACAD", ATTR{online}=="0", RUN+="${pkgs.systemd}/bin/systemctl start --no-block battery-care.service"

          # Allow USB remote wakeup (e.g. external keyboard) to resume from
          # suspend: the kernel disables wakeup on hubs by default, and the
          # whole chain (device -> external hub -> xHCI root hub) must be
          # enabled for the wake signal to propagate. Class 09 = USB hubs.
          ACTION=="add", SUBSYSTEM=="usb", ATTR{bDeviceClass}=="09", TEST=="power/wakeup", ATTR{power/wakeup}="enabled"
        '';
      };
    };
}
