{ inputs, ... }:
{
  configurations.nixos.sisko.module =
    let
      ssd = "/dev/disk/by-id/nvme-INTEL_SSDPEKKF010T8L_PHHP938405741P0D";
    in
    {
      imports = [ inputs.disko.nixosModules.default ];

      boot.zfs.extraPools = [ "tank" ];
      # ZFS datasets on the `tank` disk have been created manually
      # zfs create tank/forgejo-dumps
      # zfs create tank/forgejo-lfs
      # zfs create tank/opencloud
      # zfs create tank/paperless
      # zfs create tank/seedvault
      # zfs create tank/syncthing
      # zfs create tank/trilium
      # zfs create -o compression=off -o recordsize=1M tank/media
      # zfs create -o compression=off -o recordsize=1M tank/immich
      # zfs create -o compression=off -o recordsize=1M tank/torrent
      # zfs create -o compression=zstd tank/atticd
      # zfs create -o compression=zstd tank/ncps
      # zfs create -o compression=off tank/amule

      disko.devices = {
        disk.main = {
          type = "disk";
          device = ssd;
          content = {
            type = "gpt";
            partitions = {
              ESP = {
                size = "2G";
                type = "EF00";
                content = {
                  type = "filesystem";
                  format = "vfat";
                  mountpoint = "/boot";
                  mountOptions = [ "umask=0077" ];
                };
              };

              swap = {
                size = "16G";
                content = {
                  type = "swap";
                  discardPolicy = "both";
                };
              };

              zfs = {
                size = "100%";
                content = {
                  type = "zfs";
                  pool = "rpool";
                };
              };
            };
          };
        };

        zpool.rpool = {
          type = "zpool";
          options = {
            ashift = "12";
            autotrim = "on";
          };
          rootFsOptions = {
            compression = "zstd";
            acltype = "posixacl";
            xattr = "sa";
            dnodesize = "auto";
            normalization = "formD";
            mountpoint = "none";
            canmount = "off";
          };

          datasets = {
            "local/root" = {
              type = "zfs_fs";
              mountpoint = "/";
              options = {
                mountpoint = "legacy";
                canmount = "noauto";
              };
              postCreateHook = ''
                zfs list -t snapshot rpool/local/root@blank 2>/dev/null || \
                zfs snapshot rpool/local/root@blank
              '';
            };

            "local/nix" = {
              type = "zfs_fs";
              mountpoint = "/nix";
              options = {
                mountpoint = "legacy";
                atime = "off";
              };
            };

            "safe/home" = {
              type = "zfs_fs";
              mountpoint = "/home";
              options.mountpoint = "legacy";
            };

            "safe/persist" = {
              type = "zfs_fs";
              mountpoint = "/persist";
              options.mountpoint = "legacy";
            };
          };
        };
      };
    };

}
