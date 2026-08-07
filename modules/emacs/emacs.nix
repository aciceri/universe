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
      terraform-ts-mode = pkgs.emacsPackages.trivialBuild {
        pname = "terraform-ts-mode";
        version = "0.1.0";
        src = pkgs.fetchFromGitHub {
          owner = "kgrotel";
          repo = "terraform-ts-mode";
          rev = "28bafd1c56cfeb94c5a3f2acedc3aba2c6a6bc24";
          hash = "sha256-TTcDIxA35h08oFMQr/ichF5ANClqXIcE4NXggYxeZzo=";
        };
        packageRequires = [ ];
      };
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
        programs.emacs = {
          enable = true;
          # pgtk targets Wayland/X11; on macOS the native Mitsuharu port is the
          # right GUI build, so pick per platform.
          package = if pkgs.stdenv.hostPlatform.isDarwin then pkgs.emacs-macport else pkgs.emacs-pgtk;
          extraPackages =
            epkgs:
            let
              # hel (Helix emulation layer) and its extensions are not on MELPA
              # yet; build them from GitHub like terraform-ts-mode above. Deps
              # come from epkgs so they match this Emacs build.
              helSrc =
                {
                  pname,
                  rev,
                  hash,
                  deps ? [ ],
                }:
                epkgs.trivialBuild {
                  inherit pname;
                  version = "0-unstable-" + builtins.substring 0 7 rev;
                  src = pkgs.fetchFromGitHub {
                    owner = "anuvyklack";
                    repo = pname;
                    inherit rev hash;
                  };
                  packageRequires = deps;
                };
              hel = helSrc {
                pname = "hel";
                rev = "d58cd5dc0f2e54f5a5bf5e16230c377410557099";
                hash = "sha256-xKU0DaGBFOU1gt/u02ELgbpfXusSMdetFUX64aNaK5c=";
                deps = with epkgs; [
                  dash
                  avy
                  pcre2el
                  ultra-scroll
                ];
              };
              # hel-collection (Hel bindings for third-party packages) absorbed
              # the now-archived hel-agent-shell. Its modes/ tree is loaded by
              # path at runtime, so install it alongside the elisp instead of
              # flattening like mcp-server below.
              hel-collection = epkgs.trivialBuild rec {
                pname = "hel-collection";
                version = "0-unstable-" + builtins.substring 0 7 src.rev;
                src = pkgs.fetchFromGitHub {
                  owner = "helheim-emacs";
                  repo = "hel-collection";
                  rev = "5cbaa1b14bf476fca8724c79f710eb1a46cf38d9";
                  hash = "sha256-PWs4G3H6h1WwBabTeXnWILpaHbe8rcwg0g3Zu/MN/HY=";
                };
                packageRequires = [
                  hel
                  epkgs.dash
                ];
                postInstall = ''
                  cp -r modes $out/share/emacs/site-lisp/
                '';
              };
              # emacs-mcp-server (MCP server exposing Emacs to LLM agents) is
              # not on MELPA; build it like hel above. The tool definitions
              # live in tools/, which trivialBuild ignores, so flatten them
              # into the root first; keep the stdio wrapper scripts alongside
              # the elisp for `claude mcp add emacs .../mcp-wrapper.sh <sock>`.
              mcp-server = epkgs.trivialBuild rec {
                pname = "mcp-server";
                version = "0-unstable-" + builtins.substring 0 7 src.rev;
                src = pkgs.fetchFromGitHub {
                  owner = "rhblind";
                  repo = "emacs-mcp-server";
                  rev = "a5d749cf9880598f66308545985526fd4460627f";
                  hash = "sha256-ugaOqSnphgUKVm0+sem6oNthOFHIB5uIpksyTuGSsxE=";
                };
                preBuild = ''
                  cp tools/*.el .
                '';
                postInstall = ''
                  install -m755 mcp-wrapper.py mcp-wrapper.sh $out/share/emacs/site-lisp/
                '';
              };
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
              (helSrc {
                pname = "hel-leader";
                rev = "32230075e01749ace44ddf2d25fca0ba6aa98fbd";
                hash = "sha256-2cJxCJWwnGWLyodYU4rbnnQ3uzV6oWl+zATVniraDSw=";
                deps = [
                  hel
                  dash
                  s
                ];
              })
              hel-collection
              (helSrc {
                pname = "hel-org";
                rev = "3d7dc4e4e05533f319a05da17d8defe36d6b35b7";
                hash = "sha256-73128sCRot/B//mqZ9gRJa25a57S+T/Wg2QQlYbtUOU=";
                deps = [ hel ];
              })
              ghostel
              (helSrc {
                pname = "hel-ghostel";
                rev = "999df8dfa84cb0074e8ae739262c1cbba9e3d3f3";
                hash = "sha256-1NMGK6PBAKWdK/BCyQmmxBr2T4fx2yvU5wzbM4TSGL0=";
                deps = [
                  hel
                  ghostel
                ];
              })
              consult
              consult-project-extra
              consult-eglot
              vertico
              corfu
              nix-ts-mode
              haskell-ts-mode
              agent-shell
              mcp-server
              orderless
              marginalia
              god-mode
              flyover
              indent-bars
              treesit-grammars.with-all-grammars
              terraform-ts-mode
              catppuccin-theme
              rainbow-delimiters
              envrc
              eat
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
