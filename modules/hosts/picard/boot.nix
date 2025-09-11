fpArgs@{ inputs, ... }:
{
  configurations.nixos.picard.module =
    {
      config,
      lib,
      pkgs,
      ...
    }:
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
        loader.systemd-boot.enable = lib.mkForce false; # needed by lanzaboote
        lanzaboote = {
          enable = true;
          pkiBundle = "/var/lib/sbctl";
          configurationLimit = 20;
        };

        # To unlock and activate NixOS from SSH run
        # systemctl start initrd-nixos-activation
        initrd = {
          network = {
            ssh = {
              enable = true;
              ignoreEmptyHostKeys = true;
              extraConfig = ''
                HostKey /ssh_initrd_host_ed25519_key
              '';
              authorizedKeys = [
                # TODO add secrets.ssh_host_key_sisko
              ]
              ++ (
                fpArgs.config.users
                |> lib.attrValues
                |> lib.filter (user: user.god == true)
                |> lib.map (user: user.sshKeys)
                |> lib.flatten
              );
            };
          };
          systemd = {
            enable = true;
            network.enable = true;
            storePaths = [
              "${config.programs.ssh.package}/bin/ssh-keygen"
              "${pkgs.bashInteractive}/bin/bash"
            ];
            services.sshd.preStart = ''
              [ ! -f /ssh_initrd_host_ed25519_key ] && ${config.programs.ssh.package}/bin/ssh-keygen -t ed25519 -N "" -f /ssh_initrd_host_ed25519_key
              chmod 600 /ssh_initrd_host_ed25519_key
            '';
          };
        };
      };
    };
}
