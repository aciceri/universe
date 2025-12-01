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

    # TODO this may help with graphical artifacts after resume, remove if doesn't help
    boot.kernelParams = [
      "amdgpu.sg_display=0"
      "amdgpu.dcdebugmask=0x10"
    ];

    zramSwap = {
      enable = true;
      algorithm = "zstd";
    };

    services.zfs.autoScrub.enable = true;
  };
}
