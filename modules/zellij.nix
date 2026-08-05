{
  flake.modules.homeManager.zellij =
    {
      lib,
      pkgs,
      ...
    }:
    {
      programs.zellij = {
        enable = true;
        settings = {
          default_shell = lib.getExe pkgs.nushell;
          default_layout = "compact";
          pane_frames = false;
          show_startup_tips = false;
          copy_on_select = true;
          scroll_buffer_size = 100000;

          # Global keybinds (active in every mode except `locked`).
          # Alt/Option is reserved by the tiling WM (OmniWM), so we use Ctrl-based bindings.
          # Trade-off: Ctrl-l (clear screen) is shadowed by MoveFocusOrTab Right.
          # If you want it back, rebind clear in your shell (e.g. Ctrl-y).
          keybinds._children = [
            {
              shared_except = {
                _args = [ "locked" ];
                _children = [
                  # Panes (hjkl). MoveFocusOrTab wraps to the adjacent tab
                  # when there is no pane in that direction.
                  {
                    bind = {
                      _args = [ "Ctrl h" ];
                      MoveFocusOrTab._args = [ "Left" ];
                    };
                  }
                  {
                    bind = {
                      _args = [ "Ctrl l" ];
                      MoveFocusOrTab._args = [ "Right" ];
                    };
                  }
                  {
                    bind = {
                      _args = [ "Ctrl j" ];
                      MoveFocus._args = [ "Down" ];
                    };
                  }
                  {
                    bind = {
                      _args = [ "Ctrl k" ];
                      MoveFocus._args = [ "Up" ];
                    };
                  }

                  # Explicit tab navigation (à la VSCode).
                  {
                    bind = {
                      _args = [ "Ctrl ," ];
                      GoToPreviousTab = { };
                    };
                  }
                  {
                    bind = {
                      _args = [ "Ctrl ." ];
                      GoToNextTab = { };
                    };
                  }
                ];
              };
            }
          ];
        };
      };
    };
}
