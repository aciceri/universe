{
  configurations.nixos.pike.module = {
    fileSystems."/" = {
      device = "zpool/root";
      fsType = "zfs";
      options = [ "zfsutil" ];
    };

    fileSystems."/nix" = {
      device = "zpool/nix";
      fsType = "zfs";
      options = [ "zfsutil" ];
    };

    fileSystems."/var" = {
      device = "zpool/var";
      fsType = "zfs";
      options = [ "zfsutil" ];
    };

    fileSystems."/home" = {
      device = "zpool/home";
      fsType = "zfs";
      options = [ "zfsutil" ];
    };

    fileSystems."/boot" = {
      device = "/dev/disk/by-uuid/4AA5-7242";
      fsType = "vfat";
      options = [
        "fmask=0022"
        "dmask=0022"
      ];
    };
  };
}
