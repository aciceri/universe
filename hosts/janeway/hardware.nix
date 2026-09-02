{
  configurations.nixos.janeway.module = {
    # Generated on the machine itself by
    # `nixos-anywhere --generate-hardware-config nixos-facter`.
    facter.reportPath = ./facter.json;

    # 4 GB and no swap partition: compressed swap keeps rspamd and clamless
    # postfix from OOMing during a burst without touching the disk.
    zramSwap = {
      enable = true;
      algorithm = "zstd";
    };
  };
}
