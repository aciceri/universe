{
  flake.modules.homeManager.workstation = {
    programs.zed-editor = {
      enable = false;
      extensions = [
        "nix"
        "catppuccin"
        "haskell"
        "lean4"
        "terraform"
      ];
      userSettings = {
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
}
