{
  flake.modules.homeManager.ghostty = {
    programs.ghostty = {
      enable = true;
      # On darwin, ghostty isn't packaged in nixpkgs; we install the app via
      # Homebrew cask and let HM manage only the config file.
      # On Linux this can be overridden to install pkgs.ghostty.
      package = null;

      settings = {
        window-padding-x = 8;
        window-padding-y = 8;
        window-padding-balance = true;
        window-decoration = true;
        macos-option-as-alt = true;

        cursor-style = "block";
        cursor-style-blink = false;

        confirm-close-surface = false;
        copy-on-select = true;

        # Tabs disabled — zellij is the multiplexer of choice.
        window-new-tab-position = "end";
        macos-titlebar-style = "hidden";

        keybind = [
          "shift+enter=text:\\n"
          "cmd+t=unbind"
          "cmd+shift+t=unbind"
          "cmd+shift+]=unbind"
          "cmd+shift+[=unbind"
          # Alt+T (global): new Ghostty window from any app. Requires
          # Accessibility permission for Ghostty. Alt+T was freed up in OmniWM
          # (the Niri column-tab is now Ctrl+Alt+T).
          "global:alt+t=new_window"
        ];
      };
    };
  };
}
