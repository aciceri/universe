{
  configurations.nixos.sisko.module =
    { pkgs, lib, ... }:
    {
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

      systemd.services.console-dpms = {
        description = "Enable DPMS power down on console";
        after = [ "systemd-vconsole-setup.service" ];
        wantedBy = [ "multi-user.target" ];
        serviceConfig = {
          Type = "oneshot";
          ExecStart = "${pkgs.writeShellScript "console-dpms" ''
            TERM=linux ${lib.getExe' pkgs.util-linux "setterm"} --powerdown 1 > /dev/tty1
          ''}";
        };
      };
    };
}
