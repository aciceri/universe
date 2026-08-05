# Hammerspoon global hotkeys for things OmniWM can't do itself (it has no
# "exec/launch" or "close window" action). Alt is the OmniWM modifier, but these
# chords are left unbound in OmniWM, so Hammerspoon catches them.
#
# One-time setup: launch Hammerspoon once, grant it Accessibility, and turn on
# "Launch Hammerspoon at login" in its preferences. The app itself is already
# installed via the homebrew cask (hosts/archer/homebrew.nix).
{
  configurations.darwin.archer.module = {
    home-manager.users.ccr.home.file.".hammerspoon/init.lua".text = ''
      -- Auto-reload this config when it changes (e.g. after `nh darwin switch`
      -- rewrites it), so new bindings apply without a manual reload.
      hs.pathwatcher.new(hs.configdir, hs.reload):start()

      -- Alt+Q: close just the focused window. We close it via the window API
      -- rather than synthesizing Cmd+W: the physical Alt is still held when the
      -- synthetic keystroke fires and contaminates its modifiers (Cmd+Alt+W,
      -- etc.), which hits Ghostty's more destructive close variants
      -- (close_tab / close_all_windows) instead of close_surface.
      hs.hotkey.bind({ "alt" }, "q", function()
        local win = hs.window.focusedWindow()
        if win then win:close() end
      end)

      -- Alt+S: interactive region screenshot straight to the clipboard
      -- (macOS `screencapture -i -c`, i.e. the Cmd+Ctrl+Shift+4 variant). You
      -- get the crosshair to drag-select a region (or press Space for a window).
      -- Run via hs.task so the interactive selection doesn't block Hammerspoon's
      -- main thread. Needs Screen Recording permission granted to Hammerspoon.
      hs.hotkey.bind({ "alt" }, "s", function()
        hs.task.new("/usr/sbin/screencapture", function() end, { "-i", "-c" }):start()
      end)

      -- Alt+B: new Zen window in the running instance (shared profile/session —
      -- same logins, history, bookmarks, Zen workspaces; not a second instance).
      -- Launches Zen if it isn't running yet. Run via the default /bin/sh, NOT
      -- with_user_env=true: that routes through the login shell, which here is
      -- Nushell — it foregrounds Zen (blocking) and rejects POSIX `&`/`2>&1`.
      -- Backgrounded + output redirected so hs.execute's popen read gets EOF
      -- immediately; otherwise it waits forever on Zen's pipe and deadlocks
      -- Hammerspoon's main thread (dead menubar icon + every hotkey frozen).
      hs.hotkey.bind({ "alt" }, "b", function()
        hs.execute("nohup /Applications/Zen.app/Contents/MacOS/zen --new-window >/dev/null 2>&1 &")
      end)
    '';
  };
}
