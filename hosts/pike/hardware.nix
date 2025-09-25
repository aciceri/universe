{
  configurations.nixos.pike.module = {
    facter.reportPath = ./facter.json;

    powerManagement.cpuFreqGovernor = "schedutil";

    hardware = {
      cpu.intel.updateMicrocode = true;
      enableRedistributableFirmware = true;
      graphics = {
        enable = true;
        enable32Bit = true;
      };

      nvidia.open = true;
      nvidia.prime = {
        offload = {
          enable = true;
          enableOffloadCmd = true;
        };
        intelBusId = "PCI:0:2:0";
        nvidiaBusId = "PCI:1:0:0";
      };

      nvidia-container-toolkit.enable = true;
    };

    zramSwap = {
      enable = true;
      algorithm = "zstd";
    };

    services.zfs.autoScrub.enable = true;
    services.power-profiles-daemon.enable = true;
    services.xserver.videoDrivers = [ "nvidia" ];
  };
}
