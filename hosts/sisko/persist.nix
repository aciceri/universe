{ inputs, ... }:
{
  configurations.nixos.sisko.module = {
    imports = [ inputs.impermanence.nixosModules.impermanence ];

    environment.persistence."/persist" = {
      hideMounts = true;
      directories = [
        "/etc/NetworkManager/system-connections"
        "/var/db/dhcpcd/"
        "/var/lib/NetworkManager/"
        "/var/lib/nixos"
        "/var/lib/systemd"
        "/var/lib/systemd/coredump"
        "/var/log"
        "/var/lib/containers"
        "/var/lib/postgresql"
      ];
      files = [
        "/etc/machine-id"
        "/etc/ssh/ssh_host_ed25519_key"
        "/etc/ssh/ssh_host_ed25519_key.pub"
        "/etc/ssh/ssh_host_rsa_key"
        "/etc/ssh/ssh_host_rsa_key.pub"
      ];
    };

    age.identityPaths = [
      "/persist/etc/ssh/ssh_host_ed25519_key"
      "/persist/etc/ssh/ssh_host_rsa_key"
    ];

    fileSystems."/persist".neededForBoot = true;

    boot.tmp.cleanOnBoot = true;
  };
}
