{ lib, ... }:
{
  flake.modules.homeManager.base =
    { pkgs, ... }:
    {
      programs.helix = {
        enable = true;
        package = pkgs.helix-master;
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
              config.SentenceCapitalization = false;
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
            haskell-language-server = {
              command = "haskell-language-server";
              args = [ "--lsp" ];
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
            {
              name = "haskell";
              language-servers = [ "haskell-language-server" ];
              formatter.command = "ormolu";
            }
          ];
        };
      };

      programs.nushell.environmentVariables.EDITOR = "hx";
    };
}
