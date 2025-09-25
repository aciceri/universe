{
  configurations.nixos.sisko.module = {
    facter.reportPath = ./facter.json;

    powerManagement = {
      cpuFreqGovernor = "schedutil";
      scsiLinkPolicy = "med_power_with_dipm";
    };

    hardware.deviceTree = {
      enable = true;
      name = "rockchip/rk3588-rock-5b.dtb";
    };

    systemd.services."serial-getty@ttyS2" = {
      enable = true;
      wantedBy = [ "getty.target" ];
      serviceConfig.restart = "always";
    };
  };
}
