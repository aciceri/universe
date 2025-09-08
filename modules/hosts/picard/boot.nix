{ inputs, ... }:
{
  configurations.nixos.picard.module =
    { config, lib, ... }:
    {
      imports = [ inputs.lanzaboote.nixosModules.lanzaboote ];
      boot = {
        kernelParams = [ "ip=dhcp" ];
        initrd.kernelModules = [ "amdgpu" ];
        initrd.availableKernelModules = [
          "nvme"
          "xhci_pci"
          "ahci"
          "usbhid"
          "r8169"
        ];
        kernelModules = [
          "kvm-amd"
          "ddcci"
          "ddcci-backlight"
          "i2c-dev" # needed?
        ];

        extraModulePackages = [
          config.boot.kernelPackages.ddcci-driver
        ];

        loader.efi.canTouchEfiVariables = true;
        loader.systemd-boot = {
          enable = lib.mkForce false; # needed by lanzaboote
        };
        lanzaboote = {
          enable = true;
          pkiBundle = "/etc/secure";
          configurationLimit = 20;
        };
      };
    };
}
