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
    in
    lib.mkMerge [
      {
        home.packages = with pkgs; [
          claude-agent-acp
          ruff
          nixd
          vtsls
          terraform-ls
        ];

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
                rev = "ed555d96e0373a9f67a0e292993db6c72fb5a521";
                hash = "sha256-cx3tDEYuTwrVVFOKyzdrXiL3SeG0PcJhcDZkVnRXIu8=";
                deps = with epkgs; [
                  dash
                  avy
                  pcre2el
                ];
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
                rev = "9e7bd67d6e1ce0915bfd5f2341eb0b9ea5217bbf";
                hash = "sha256-uJ684ik1hUeRQv6uQPQx7urKfo3yqqt4X3dHwnUxGlI=";
                deps = [
                  hel
                  dash
                  s
                ];
              })
              (helSrc {
                pname = "hel-agent-shell";
                rev = "78156b5090bfb35d0562cf426715244cbf83df55";
                hash = "sha256-JlJHADhVBu5FytprO58wpLXBm1ejuGnAQDSA8MzohLg=";
                deps = [
                  hel
                  agent-shell
                  dash
                ];
              })
              (helSrc {
                pname = "hel-org";
                rev = "5748f19ea3e46860a8cdb475d1241926808a290a";
                hash = "sha256-17xrWnJBYCSe1+yKv3Qbj95yMy/XMZ4VwpZOM7I9SCE=";
                deps = [ hel ];
              })
              ghostel
              (helSrc {
                pname = "hel-ghostel";
                rev = "cb324661d6817a140d4447a85a634478f66deaac";
                hash = "sha256-qmArbZhYcYArlHSR/bl880UKfQUnQXJ3lzBDQjpHZTE=";
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
    ];
}
