{
  configurations.nixos.sisko.module =
    { pkgs, ... }:
    {
      facter.reportPath = ./facter.json;

      powerManagement.cpuFreqGovernor = "schedutil";

      zramSwap = {
        enable = true;
        memoryPercent = 100;
        algorithm = "zstd";
      };

      boot.kernelParams = [ "usb-storage.quirks=0bda:9201:u" ];

      boot.kernel.sysctl = {
        "vm.swappiness" = 100;
      };

      services.logind.settings.Login = {
        HandleLidSwitchExternalPower = "ignore";
        HandleLidSwitch = "ignore";
      };

      services.zfs.autoScrub = {
        enable = true;
        interval = "monthly";
        pools = [
          "rpool"
          "tank"
        ];
      };

      services.zfs.autoSnapshot = {
        enable = true;
        monthly = 3;
        weekly = 4;
        daily = 7;
        hourly = 24;
        frequent = 4;
      };

      hardware.graphics = {
        enable = true;
        extraPackages = with pkgs; [
          intel-media-driver
          intel-compute-runtime
        ];
      };
    };
}
