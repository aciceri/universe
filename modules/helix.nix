{ lib, ... }:
{
  flake.modules.homeManager.base =
    { pkgs, config, ... }:
    let
      steelixDarwin = pkgs.steelix.overrideAttrs {
        patches = [
          (pkgs.fetchpatch {
            name = "revert-dylib-grammar-extension.patch";
            url = "https://github.com/helix-editor/helix/commit/430914b298a32653ab1847fdfdf2177a002be04c.patch";
            revert = true;
            hash = "sha256-4KUFppkso4/XwNU+mGIgLvl+mJXHZWkmaguYMy8oTyI=";
          })
        ];
      };
    in
    {
      programs.helix = {
        enable = true;
        package = if pkgs.stdenv.isDarwin then steelixDarwin else pkgs.steelix;
        defaultEditor = true; # doesn't seem to work with nushell
        settings = {
          # Stylix's base16 theme paints "hint" with base03, the same color as
          # comments, so hint-severity virtual text (harper-ls) is unreadable.
          theme = lib.mkForce "stylix-hints";
          editor = {
            mouse = true;
            middle-click-paste = true;
            scroll-lines = 3;
            color-modes = true;
            true-color = true;
            rulers = [ ];
            bufferline = "never";
            cursorline = true;
            line-number = "relative";
            rainbow-brackets = true;
            completion-replace = true;
            trim-trailing-whitespace = true;
            end-of-line-diagnostics = "hint";

            inline-diagnostics.cursor-line = "warning";

            soft-wrap.enable = true;

            auto-save = {
              focus-lost = true;
              after-delay.enable = true;
              after-delay.timeout = 300000;
            };

            cursor-shape = {
              insert = "bar";
              normal = "block";
              select = "underline";
            };

            statusline = {
              left = [
                "mode"
                "spinner"
                "version-control"
                "spacer"
                "separator"
                "file-name"
                "read-only-indicator"
                "file-modification-indicator"
              ];
              center = [ ];
              right = [
                "diagnostics"
                "workspace-diagnostics"
                "position"
                "total-line-numbers"
                "position-percentage"
                "file-encoding"
                "file-line-ending"
                "file-type"
                "register"
                "selections"
              ];
              separator = "│";
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
          language-server = {
            harper-ls = {
              command = lib.getExe pkgs.harper;
              args = [ "--stdio" ];
              config.harper-ls.linters.SentenceCapitalization = false;
            };
            nixd = {
              command = lib.getExe pkgs.nixd;
              args = [
                "--inlay-hints"
                "--semantic-tokens"
              ];
            };
            vtsls = {
              command = lib.getExe pkgs.vtsls;
              args = [ "--stdio" ];
            };
          };

          language = [
            {
              name = "nix";
              language-servers = [
                "nixd"
                "harper-ls"
              ];
              formatter.command = "nixfmt";
            }
            {
              name = "typescript";
              language-servers = [ "vtsls" ];
            }
          ];
        };

        themes.stylix-hints =
          let
            teal = config.lib.stylix.colors.withHashtag.base0C;
          in
          {
            inherits = "stylix";
            hint = teal;
            "diagnostic.hint".underline = {
              style = "curl";
              color = teal;
            };
          };
      };

      programs.nushell.environmentVariables.EDITOR = "hx";
    };
}
