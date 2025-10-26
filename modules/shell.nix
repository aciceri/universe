{
  flake.modules.nixos.base =
    { pkgs, ... }:
    {
      users.defaultUserShell = pkgs.nushell;
      programs.mosh.enable = true;
    };

  flake.modules.homeManager.base =
    {
      config,
      pkgs,
      lib,
      ...
    }:
    {
      programs = {
        nushell = {
          enable = true;
          settings = {
            show_banner = false;
          };
          environmentVariables = {
            PAGER = lib.getExe config.programs.bat.package;
          };
          plugins = with pkgs.nushellPlugins; [
            gstat
            query
          ];
          extraConfig = # nushell
            ''
              $env.PROMPT_INDICATOR = {||
                if $env.LAST_EXIT_CODE != 0 {
                  $"(ansi red)〉(ansi reset)"
                } else {
                  "〉"
                }
              }

              $env.PROMPT_COMMAND_RIGHT = {||
                let user = (whoami)
                let host = (hostname)
                let time = (date now | format date "%H:%M:%S")
                $"(ansi green)($user)(ansi reset)(ansi cyan)@(ansi reset)(ansi green)($host)(ansi reset) (ansi cyan)($time)(ansi reset)"
              }
            '';
        };

        carapace.enable = true;

        bat = {
          enable = true;
          extraPackages = with pkgs.bat-extras; [
            batdiff
            batman
            (batgrep.overrideAttrs {
              # FIXME https://github.com/NixOS/nixpkgs/issues/454391
              doCheck = false;
            })
            batwatch
          ];
        };

        fzf.enable = true;

        fd.enable = true;

        ripgrep.enable = true;

        ripgrep-all.enable = true;

        broot.enable = true;

        direnv = {
          enable = true;
          config = {
            warn_timeout = "60s";
          };
          nix-direnv.enable = true;
        };

        pay-respects.enable = true;

        television.enable = true;

        ranger = {
          enable = true;
          settings = {
            "preview_images" = true;
            "preview_images_method" = "sixel";
          };
        };

        zoxide.enable = true;

        btop = {
          enable = true;
          settings.update_ms = 100;
        };

        jq.enable = true;

        jqp.enable = true;
      };

      home.packages = with pkgs; [
        repgrep
        tokei
        fx
        dust
      ];
    };
}
