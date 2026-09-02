{ inputs, ... }:
{
  configurations.nixos.janeway.module = {
    imports = [ inputs.disko.nixosModules.default ];

    # Hetzner Cloud x86 instances are SeaBIOS, so GRUB goes in a 1 MiB BIOS
    # boot partition and there is no ESP.
    #
    # The root filesystem is a tmpfs: everything that survives a reboot is
    # either in the store or under /persist, and nothing else can quietly
    # accumulate (see persist.nix). btrfs subvolumes rather than two fixed
    # partitions so /nix and /persist share the 40 GB without a wall between
    # them.
    disko.devices = {
      nodev."/" = {
        fsType = "tmpfs";
        mountOptions = [
          "size=1G"
          "mode=755"
        ];
      };

      disk.main = {
        type = "disk";
        device = "/dev/sda";
        content = {
          type = "gpt";
          partitions = {
            boot = {
              size = "1M";
              type = "EF02";
            };
            # GRUB's own files cannot live on the tmpfs root, and putting
            # /boot on btrfs is asking for trouble, so it gets a small plain
            # partition of its own.
            grub = {
              size = "512M";
              content = {
                type = "filesystem";
                format = "ext4";
                mountpoint = "/boot";
              };
            };
            root = {
              size = "100%";
              content = {
                type = "btrfs";
                extraArgs = [ "-f" ];
                subvolumes = {
                  "/nix" = {
                    mountpoint = "/nix";
                    mountOptions = [
                      "compress=zstd"
                      "noatime"
                    ];
                  };
                  "/persist" = {
                    mountpoint = "/persist";
                    mountOptions = [
                      "compress=zstd"
                      "noatime"
                    ];
                  };
                };
              };
            };
          };
        };
      };
    };
  };
}
