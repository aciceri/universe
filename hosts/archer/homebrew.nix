{
  configurations.darwin.archer.module = {
    homebrew = {
      casks = [
        "zen"
        "slack"
        "ghostty"
        "claude"
        "lm-studio"
        "linear"
        "loom"
        "google-chrome"
        "zed"
        "telegram"
        "whatsapp"
        "BarutSRB/tap/omniwm"
        "discord"
        "element"
        "monitorcontrol"
        "signal"
        "blender"
        "opencloud"
        "trilium-notes"
        "hammerspoon"
        "karabiner-elements"
        "calibre"
        "tor-browser"
      ];

      brews = [
        "mas"
        "mlx-lm"
        "startergo/qemu-virgl-kosmickrisp/qemu"
      ];

      # No `masApps`: every `mas` operation (install/upgrade/uninstall) needs an
      # interactive context and deadlocks (SIGTTIN) under the non-interactive
      # `brew bundle` activation. With zero mas entries, `brew bundle` never
      # touches the Mac App Store (cleanup short-circuits on an empty mas list),
      # so installed apps — WireGuard, GarageBand, iMovie, Keynote, Numbers,
      # Pages — are left untouched. Manage Mac App Store apps from the App Store.
    };
  };
}
