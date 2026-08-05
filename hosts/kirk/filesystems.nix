{
  configurations.nixos.kirk.module = {
    # Placeholder so the bare toplevel evaluates (and the flake check builds):
    # kirk only ever runs as a QEMU VM (modules/darwin-vm.nix), where the
    # qemu-vm module overrides the root filesystem and bootloader anyway.
    fileSystems."/" = {
      device = "/dev/disk/by-label/nixos";
      fsType = "ext4";
    };
    boot.loader.grub.device = "nodev";
  };
}
