{
  # nix-darwin has no services.ollama module; we install the official app via
  # Homebrew cask which bundles a menubar launcher + auto-start.
  flake.modules.darwin.ollama = {
    homebrew.casks = [
      "ollama-app"
    ];
  };
}
