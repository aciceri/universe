{ lib, inputs, ... }:
let
  allowedUnfree = [
    "apple_cursor"
    "claude-code"
    "slack"
    "zerotierone"
    "google-chrome"
    "nvidia-x11"
    "nvidia-settings"
    "nvidia-dc"
    "fabricmanager"
    "android-studio"
    "android-studio-stable"
    "android-studio-tools"
    "brscan4"
    "brother-udev-rule-type1"
    "brscan4-etc-files"
    "spotify"
    "cursor-cli"
    "open-webui"
    "antigravity-cli"
    "geosurge"
  ];
  unfreeConfig = {
    nixpkgs.config = {
      allowUnfreePredicate = pkg: lib.elem (lib.getName pkg) allowedUnfree;
      nvidia.acceptLicense = true;
    };
  };
in
{
  flake.modules.nixos.base = unfreeConfig;
  flake.modules.darwin.base = unfreeConfig;

  # The flake's own package set needs the same policy: packages defined here
  # are built through `nix build .#<name>` and their CI checks, which do not go
  # through a NixOS/Darwin configuration and would otherwise hit the unfree
  # meta check (e.g. geosurge).
  perSystem =
    { system, ... }:
    {
      _module.args.pkgs = import inputs.nixpkgs {
        inherit system;
        config.allowUnfreePredicate = pkg: lib.elem (lib.getName pkg) allowedUnfree;
      };
    };
}
