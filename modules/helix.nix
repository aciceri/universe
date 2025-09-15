{
  flake.modules.homeManager.base = {
    programs.helix = {
      enable = true;
      defaultEditor = true; # doesn't seem to work with nushell
      settings = {
        editor = {
          mouse = true;
          middle-click-paste = true;
          scroll-lines = 3;
          color-modes = true;
          true-color = true;
          rulers = [ ];
          bufferline = "never";
          cursorline = true;
          auto-save = false;
          line-number = "relative";

          cursor-shape = {
            insert = "bar";
            normal = "block";
            select = "underline";
          };

          statusline = {
            left = [
              "mode"
              "spinner"
              "file-name"
            ];
            center = [ ];
            right = [
              "diagnostics"
              "selections"
              "position"
              "file-encoding"
              "file-line-ending"
            ];
          };

          lsp = {
            display-messages = true;
            auto-signature-help = true;
            display-inlay-hints = true;
          };

          file-picker = {
            hidden = true;
            git-ignore = true;
          };

          indent-guides = {
            render = true;
          };
        };
      };
      languages = {
        language = [
          {
            name = "nix";
            language-servers = [ "nixd" ];
            formatter.command = "nixfmt";
          }
          {
            name = "typescript";
            language-servers = [ "vtsls" ];
          }
        ];
        language-server = {
          nixd.command = "nixd";
        };
      };
    };

    programs.nushell.environmentVariables.EDITOR = "hx";
  };
}
