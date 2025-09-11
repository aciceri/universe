{
  configurations.nixos.picard.module =
    { config, ... }:
    {
      boot = {
        kernelModules = [
          "ddci"
          "ddci-backlight"
          "i2c-dev" # needed?
        ];

        extraModulePackages = [ config.boot.kernelPackages.ddcci-driver ];
      };

      # Workaround, shouldn't be necessary
      systemd.services.ddcci = {
        script = "echo 'ddcci 0x37' > /sys/bus/i2c/devices/i2c-2/new_device";
        wantedBy = [ "graphical.target" ];
        restartIfChanged = false;
        serviceConfig = {
          Type = "oneshot";
          RemainAfterExit = true;
        };
      };
    };
}
