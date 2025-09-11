{
  configurations.nixos.picard.module = {
    facter.reportPath = ./facter.json;

    powerManagement.cpuFreqGovernor = "schedutil";

    hardware = {
      cpu.amd.updateMicrocode = true;
      enableRedistributableFirmware = true;
      graphics = {
        enable = true;
        enable32Bit = true;
      };
    };

    zramSwap = {
      enable = true;
      algorithm = "zstd";
    };
  };
}
