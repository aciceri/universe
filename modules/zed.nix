{ config, ... }:
{
  flake.modules.homeManager.zed =
    { pkgs, ... }:
    {
      programs.zed-editor = {
        enable = true;
        # On darwin the binary comes from a Homebrew cask; HM only writes config.
        package = if pkgs.stdenv.hostPlatform.isLinux then pkgs.zed-editor else null;
        extensions = [
          "nix"
          "catppuccin"
          "haskell"
          "lean4"
          "terraform"
        ];
        userSettings = {
          semantic_tokens = "combined";
          features = {
            copilot = false;
          };
          telemetry = {
            metrics = false;
          };
          helix_mode = true;
        };
      };
    };

  flake.modules.homeManager.workstation.imports = with config.flake.modules.homeManager; [
    zed
  ];
}
