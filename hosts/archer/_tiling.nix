# PARKED (the `_` prefix keeps this file out of the auto-import): native macOS
# window tiling (Tahoe), the pre-OmniWM setup. OmniWM is currently active
# (hosts/archer/omniwm.nix + homebrew.nix); to switch back, rename this file to
# drop the `_` prefix and remove omniwm.nix. NOTE: this file's
# `home.file.".hammerspoon/init.lua"` conflicts with hammerspoon.nix — remove
# one of the two definitions when re-activating.
{
  configurations.darwin.archer.module = {
    system.defaults = {
      WindowManager = {
        EnableTiledWindowMargins = true; # keep a small gap between tiled windows
        EnableTilingByEdgeDrag = true; # drag a window to a side edge to tile it
        EnableTopTilingByEdgeDrag = true; # drag to the top edge to fill the screen
        EnableTilingOptionAccelerator = true; # hold ⌥ while dragging for precise tiling
      };

      # Activating an app (Spotlight, Dock, ⌘-Tab) jumps to the Space that
      # already has one of its windows open instead of moving the window.
      NSGlobalDomain.AppleSpacesSwitchOnActivate = true;

      # Keep desktops in a fixed order so their numbers (and ⌘⌃←/→) stay
      # predictable, instead of macOS shuffling them by most-recent use.
      dock.mru-spaces = false;

      # Snappier Mission Control overview (⌃↑); does not affect the ⌃←/→ slide.
      dock.expose-animation-duration = 0.1;

      # "Reopen windows when logging back in" defaults to on, so apps (and
      # their desktop assignments) come back after a restart. User-domain
      # pref, hence CustomUserPreferences rather than the loginwindow option
      # (which writes the system domain).
      CustomUserPreferences."com.apple.loginwindow".TALLogoutSavesState = true;

      # The built-in tiling shortcuts use the Globe (fn) key, which this
      # external keyboard doesn't have, so we re-map the Window-menu items to
      # ⌘⌃ chords on an IJKL inverted-T (I=up J=left K=down L=right) with the
      # quarters on the surrounding keys:
      #
      #     U I O        ⌘⌃ + …
      #     J K L   →    corners = quarters, inner = halves
      #     M , .
      #
      # NSUserKeyEquivalents has no typed nix-darwin option, so it goes through
      # CustomUserPreferences on the global domain. This *replaces* the native
      # fn+⌃+arrow shortcut on each item (a menu item holds a single key
      # equivalent), so ⌘⌃ is the one chord on both the external and the
      # built-in keyboard. Matching is by *leaf* menu-item title (the macOS 26
      # "Window->Move & Resize->Left" path form is silently ignored) and
      # applies to every app exposing such an item. Sigils: @=⌘ ~=⌥ ^=⌃ $=⇧.
      CustomUserPreferences."NSGlobalDomain".NSUserKeyEquivalents = {
        # Halves
        "Top" = "@^i";
        "Left" = "@^j";
        "Bottom" = "@^k";
        "Right" = "@^l";

        # Quarters (corners around the IJKL cluster)
        "Top Left" = "@^u";
        "Top Right" = "@^o";
        "Bottom Left" = "@^m";
        "Bottom Right" = "@^.";

        # Whole screen / restore
        "Fill" = "@^f";
        "Center" = "@^c";
        "Return to Previous Size" = "@^r";
      };
    };

    # App-focus hotkeys via Hammerspoon. Each chord activates an app; since the
    # app is maximized on its own desktop, this jumps you there (with
    # AppleSpacesSwitchOnActivate doing the Space switch). Hammerspoon uses
    # Carbon hotkeys (RegisterEventHotKey), not a CGEvent tap like skhd — which
    # never fired on this machine — and its Accessibility grant is stable across
    # updates (a signed .app in /Applications, not a changing nix-store path).
    #
    # One-time setup: launch Hammerspoon once and grant it Accessibility.
    home-manager.users.ccr.home.file.".hammerspoon/init.lua".text = ''
      -- Focus: jump to the app's window (and its Space).
      hs.hotkey.bind({ "cmd", "ctrl" }, "t", function() hs.application.launchOrFocus("Ghostty") end)
      hs.hotkey.bind({ "cmd", "ctrl" }, "s", function() hs.application.launchOrFocus("Slack") end)
      hs.hotkey.bind({ "cmd", "ctrl" }, "b", function() hs.application.launchOrFocus("Zen") end)

      -- New separate window on the *current* Space (no jump).
      -- Ghostty: macOS has no "new window in running instance" CLI, so this
      -- spawns a fresh instance (fine — zellij auto-starts in it).
      hs.hotkey.bind({ "cmd", "ctrl", "shift" }, "t", function() hs.execute("open -na Ghostty", true) end)
      -- Zen: opens a new window in the running instance (Firefox --new-window).
      hs.hotkey.bind({ "cmd", "ctrl", "shift" }, "b", function() hs.execute("/Applications/Zen.app/Contents/MacOS/zen --new-window", true) end)
    '';
  };
}
