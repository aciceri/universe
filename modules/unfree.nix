{ lib, ... }:
{
  flake.modules.nixos.base = {
    nixpkgs.config.allowUnfreePredicate =
      pkg:
      lib.elem (lib.getName pkg) [
        "apple_cursor"
        "claude-code"
        "discord"
        "slack"
        "zerotierone"
        "google-chrome"
      ];
  };
}
