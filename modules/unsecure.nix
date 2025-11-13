{
  flake.modules.nixos.base = {
    nixpkgs.config.permittedInsecurePackages = [
      "jitsi-meet-1.0.8792" # FIXME Needed by element-desktop
    ];
  };
}
