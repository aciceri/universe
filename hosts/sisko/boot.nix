{
  configurations.nixos.sisko.module = {
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
  };
}
