{
  flake.modules.nixos.base =
    { pkgs, ... }:
    {
      users.defaultUserShell = pkgs.nushell;
      programs.mosh.enable = true;
    };

  flake.modules.darwin.base =
    { pkgs, ... }:
    {
      environment.shells = [ pkgs.nushell ];

      # nushell as login shell needs PATH explicitly seeded (it doesn't source
      # /etc/zshenv where nix-darwin writes environment.systemPath).
      # Also: nushell on macOS reads from ~/Library/Application Support/nushell
      # by default, but HM writes to ~/.config/nushell — symlink them.
      home-manager.sharedModules = [
        (
          {
            osConfig,
            config,
            lib,
            ...
          }:
          {
            programs.nushell.extraEnv = ''
              ${lib.concatStringsSep "\n" (
                lib.mapAttrsToList (name: value: ''$env.${name} = "${toString value}"'') osConfig.environment.variables
              )}

              $env.PATH = (
                "${osConfig.environment.systemPath}" | split row ":"
                | append $"/etc/profiles/per-user/(whoami)/bin"
                | append $"($env.HOME)/.nix-profile/bin"
                | append "/opt/homebrew/bin"
                | append "/opt/homebrew/sbin"
                | append ($env.PATH | split row (char esep))
                | uniq
              )
            '';

            home.file."Library/Application Support/nushell".source =
              config.lib.file.mkOutOfStoreSymlink "${config.xdg.configHome}/nushell";
          }
        )
      ];
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
        direnv = {
          enable = true;
          # direnv's test suite occasionally hangs on darwin builders.
          package = pkgs.direnv.overrideAttrs (_: {
            doCheck = false;
          });
          config = {
            warn_timeout = "60s";
          };
          nix-direnv.enable = true;
        };

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
            # highlight # FIXME broken
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

              # user/host are static: resolve them once at startup. Running
              # externals (whoami/hostname) inside the closure resets
              # LAST_EXIT_CODE on every repaint, wiping the red error
              # indicator as soon as you start typing.
              let __prompt_user = (whoami)
              let __prompt_host = (hostname)
              $env.PROMPT_COMMAND_RIGHT = {||
                let time = (date now | format date "%H:%M:%S")
                $"(ansi green)($__prompt_user)(ansi reset)(ansi cyan)@(ansi reset)(ansi green)($__prompt_host)(ansi reset) (ansi cyan)($time)(ansi reset)"
              }
            '';
        };

        # Wraps nix-shell/nix develop/nix shell so they drop into nushell
        # instead of bash. Nushell integration is enabled by default.
        nix-your-shell.enable = true;

        carapace.enable = true;

        bat = {
          enable = true;
          extraPackages = with pkgs.bat-extras; [
            batdiff
            batman
            # (batgrep.overrideAttrs {
            #   # FIXME https://github.com/NixOS/nixpkgs/issues/454391
            #   doCheck = false;
            # })
            batgrep
            batwatch
          ];
        };

        fzf.enable = true;

        fd.enable = true;

        ripgrep.enable = true;

        ripgrep-all.enable = true;

        broot.enable = true;

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
