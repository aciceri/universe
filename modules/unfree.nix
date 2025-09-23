{ lib, ... }:
{
  flake.modules.nixos.base = {
    nixpkgs.config.allowUnfreePredicate =
      pkg:
      lib.elem (lib.getName pkg) [
        "apple_cursor"
        "claude-code"
        "slack"
        "zerotierone"
        "google-chrome"
        "nvidia-x11"
        "nvidia-settings"
      ];
  };
}
