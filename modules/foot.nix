{
  flake.modules.homeManager.pc = {
    programs.foot = {
      enable = true;
      settings = {
        main = {
          login-shell = "yes";
          dpi-aware = "no";
          horizontal-letter-offset = "1";
          pad = "1x1";
        };
        cursor = {
          blink = true;
        };
        tweak = {
          overflowing-glyphs = true;
        };

        key-bindings = {
          scrollback-up-page = "Control+Shift+k";
          scrollback-down-page = "Control+Shift+j";
          search-start = "Control+Shift+s";
        };

        mouse = {
          hide-when-typing = "yes";
        };
      };
    };
  };
}
