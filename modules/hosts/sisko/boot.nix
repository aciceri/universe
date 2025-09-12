{
  configurations.nixos.sisko.module = {
    boot = {
      loader = {
        systemd-boot.enable = true;
        efi.canTouchEfiVariables = false;
        systemd-boot.installDeviceTree = true;
      };

      initrd.availableKernelModules = [
        "nvme"
        "xhci_pci"
        "ahci"
        "usb_storage"
      ];

      kernelParams = [
        "earlycon"
        "consoleblank=0"
        "console=tty1"
        "console=ttyS2,1500000"
      ];
    };
  };
}
