{ config, ... }:
{
  configurations.darwin.archer.module = {
    imports = with config.flake.modules.darwin; [
      base
      claude-code-overlay
      ds4
      homebrew
      ollama
      spicetify
    ];
    home-manager.sharedModules = with config.flake.modules.homeManager; [
      base
      claude-code
      development
      # emacs  # off, see modules/emacs/emacs.nix
      ghostty
      zed
      zellij
    ];
  };
}
