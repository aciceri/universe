{ config, ... }:
{
  flake.modules.homeManager.workstation.imports = [ config.flake.modules.homeManager.emacs ];

  flake.modules.homeManager.emacs =
    {
      config,
      pkgs,
      lib,
      ...
    }:
    let
      # Bridges an MCP client's stdio to the Emacs MCP server socket.
      # init.el exports EMACS_MCP_SOCKET to every Emacs subprocess, so omp
      # launched from agent-shell (or any shell inside Emacs) connects
      # automatically; outside Emacs it fails fast.
      emacs-mcp-stdio = pkgs.writeShellScriptBin "emacs-mcp-stdio" ''
        if [ ! -S "''${EMACS_MCP_SOCKET:-}" ]; then
          echo "emacs-mcp-stdio: EMACS_MCP_SOCKET unset or stale (not inside Emacs?)" >&2
          exit 1
        fi
        exec ${lib.getExe pkgs.socat} STDIO "UNIX-CONNECT:$EMACS_MCP_SOCKET"
      '';
    in
    lib.mkMerge [
      {
        home.packages = with pkgs; [
          claude-agent-acp
          ruff
          nixd
          vtsls
          terraform-ls
          emacs-mcp-stdio
        ];

        # catppuccin-theme (loaded from init.el) is far better curated for
        # Emacs than stylix's generated base16 theme, which would load later
        # and shadow it. Font is set in init.el too.
        stylix.targets.emacs.enable = false;

        home.file.".config/emacs/init.el".source =
          config.lib.file.mkOutOfStoreSymlink "${config.universePath}/modules/emacs/init.el";
        home.file.".config/emacs/lisp".source = config.lib.file.mkOutOfStoreSymlink "${config.universePath}/modules/emacs/lisp";
        programs.emacs = {
          enable = true;
          # pgtk targets Wayland/X11; on macOS the native Mitsuharu port is the
          # right GUI build, so pick per platform.
          package = if pkgs.stdenv.hostPlatform.isDarwin then pkgs.emacs-macport else pkgs.emacs-pgtk;
          extraPackages =
            epkgs:
            let
              # These emacs-lisp packages are not on MELPA, so they are defined
              # under packages/ and built from source. Passing `epkgs` as
              # `emacsPackages` compiles them (and resolves their elisp deps)
              # against this very Emacs build.
              callEmacsPackage =
                name: args: pkgs.callPackage (../../packages + "/${name}/_package.nix") ({ emacsPackages = epkgs; } // args);
              # Instantiated once and threaded into every extension, so the
              # whole tree shares a single hel.
              hel = callEmacsPackage "emacs-hel" { };
              # ghostel's zig build locates the macOS SDK via xcode-select/xcrun,
              # which nix's sandbox lacks; xcbuild provides both (same fix as
              # nixpkgs master). Linux uses the unmodified package.
              ghostel =
                if pkgs.stdenv.hostPlatform.isDarwin then
                  let
                    module = epkgs.ghostel.module.overrideAttrs (old: {
                      nativeBuildInputs = old.nativeBuildInputs ++ [ pkgs.xcbuild ];
                    });
                  in
                  epkgs.ghostel.overrideAttrs (_: {
                    preBuild = ''
                      install ${module}/ghostel-module.dylib ghostel-module.dylib
                    '';
                  })
                else
                  epkgs.ghostel;
            in
            with epkgs;
            [
              hel
              (callEmacsPackage "emacs-hel-leader" { inherit hel; })
              (callEmacsPackage "emacs-hel-collection" { inherit hel; })
              (callEmacsPackage "emacs-hel-org" { inherit hel; })
              ghostel
              (callEmacsPackage "emacs-hel-ghostel" { inherit hel ghostel; })
              consult
              consult-project-extra
              consult-eglot
              vertico
              (callEmacsPackage "emacs-vertico-buffer-frame" { })
              corfu
              nix-ts-mode
              haskell-ts-mode
              agent-shell
              (callEmacsPackage "emacs-mcp-server" { })
              (callEmacsPackage "emacs-niri-awareness" { })
              orderless
              marginalia
              god-mode
              flyover
              indent-bars
              treesit-grammars.with-all-grammars
              (callEmacsPackage "emacs-terraform-ts-mode" { })
              catppuccin-theme
              rainbow-delimiters
              envrc
              nael
            ];
        };
      }
      (lib.mkIf pkgs.stdenv.hostPlatform.isDarwin {
        # Launch Emacs as a GUI app at login (init.el runs server-start), NOT a
        # headless launchd --fg-daemon. OmniWM only tiles windows of a
        # GUI-launched ("open") instance; a daemon's frames stay floating. With
        # `open` both the initial frame and later emacsclient/`ec` frames tile.
        launchd.agents.emacs = {
          enable = true;
          config = {
            ProgramArguments = [
              "/usr/bin/open"
              "-n"
              "${config.programs.emacs.finalPackage}/Applications/Emacs.app"
            ];
            RunAtLoad = true;
            KeepAlive = false;
          };
        };
        # The Mac port's `emacsclient -c` opens a *tty* frame by default; force a
        # native GUI frame. `ec` = new window, `ec file…` = new window on files.
        home.packages = [
          (pkgs.writeShellScriptBin "ec" ''
            exec ${config.programs.emacs.finalPackage}/bin/emacsclient -c -n -F '((window-system . mac))' "$@"
          '')
        ];
      })
      (lib.mkIf pkgs.stdenv.hostPlatform.isLinux {
        # Run Emacs as a systemd user daemon; niri's Mod+X spawns
        # `emacsclient -c` against it. init.el's server-start guard skips
        # starting a second server inside the daemon's client frames.
        services.emacs = {
          enable = true;
          startWithUserSession = "graphical";
          client.enable = true;
        };
      })
    ];
}
