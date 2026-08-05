# Desktop cosmetics for the clamshell + external-display setup: a minimal,
# snappy, keyboard-driven look that pairs with OmniWM (whose workspace bar
# already sits over the menu-bar / notch strip). Everything here is declarative
# and reversible by deleting a line. A few keys (menu-bar hide, reduce-motion)
# only fully apply after a logout/login; the rest land on `nh darwin switch`
# (Dock/Finder are restarted by the activation).
{
  configurations.darwin.archer.module = {
    system.defaults = {
      dock = {
        autohide = true;
        autohide-delay = 0.0; # no delay before the Dock slides in
        autohide-time-modifier = 0.0; # instant slide, no animation
        show-recents = false; # drop the recent-apps section
        launchanim = false; # no bouncing icon on launch
        mineffect = "scale"; # snappier than the genie minimize
        tilesize = 36;

        # Disable all four hot corners (1 = no-op) so Mission Control / Quick
        # Note never fire from a stray cursor.
        wvous-tl-corner = 1;
        wvous-tr-corner = 1;
        wvous-bl-corner = 1;
        wvous-br-corner = 1;
      };

      NSGlobalDomain = {
        # Hide the native menu bar (reveals on hover at the top edge); OmniWM's
        # workspace bar already occupies that strip.
        _HIHideMenuBar = true;

        # Kill AppKit's open/close window animation -> tiling feels instant.
        NSAutomaticWindowAnimationsEnabled = false;
      };

      # Empty wallpaper: no desktop icons, no desktop widgets, and clicking the
      # wallpaper does NOT shove tiled windows aside to reveal the desktop.
      finder.CreateDesktop = false;
      WindowManager = {
        StandardHideDesktopIcons = true;
        StandardHideWidgets = true;
        EnableStandardClickToShowDesktop = false;
      };

      # Emacs (macport) renders text with AppKit font smoothing, which fattens
      # strokes vs Ghostty's rasterizer despite the identical font. Turn it off
      # for the Emacs bundle only so both match. Takes effect on app restart.
      CustomUserPreferences."org.gnu.Emacs".AppleFontSmoothing = 0;
    };
  };
}
