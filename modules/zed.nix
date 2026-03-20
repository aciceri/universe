{
  flake.modules.homeManager.workstation = {
    programs.zed-editor = {
      enable = true;
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
}
