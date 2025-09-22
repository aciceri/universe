fpArgs: {
  configurations.nixos.pike.module =
    {
      config,
      lib,
      pkgs,
      ...
    }:
    {
      boot = {
        kernelParams = [ "ip=dhcp" ];

        initrd.kernelModules = [ "amdgpu" ];
        initrd.availableKernelModules = [
          "nvme"
          "xhci_pci"
          "usbhid"
          "thunderbolt"
          "vmd"
          "usb_storage"
          "sd_mod"
        ];
        kernelModules = [
          "kvm-amd"
          "amdgpu"
        ];

        loader.efi.canTouchEfiVariables = true;
        loader.systemd-boot.enable = lib.mkForce true;

        # TODO This is used both by by pike and picard, deduplicate it
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
