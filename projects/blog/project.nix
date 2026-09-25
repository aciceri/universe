{ getSystem, getCurrentDir, ... }:
let
  currentDir = getCurrentDir __curPos;
in
{
  gitignore =
    [
      "_cache"
      "out"
      "generator/dist-newstyle"
      "generator/thirdparty"
    ]
    |> map (p: "${currentDir}/${p}");

  perSystem =
    {
      config,
      pkgs,
      lib,
      ...
    }:
    let
      thirdparty =
        [
          (pkgs.runCommand "katex" { } ''
            cp -r ${pkgs.katex}/lib/node_modules/katex/dist $out
          '')
          (pkgs.runCommand "asciinema-player" { } ''
            cp -r ${config.packages.asciinema-player}/lib/asciinema-player/bundle $out
          '')
          (pkgs.runCommand "hyphenopoly" { } ''
            cp -r ${config.packages.hyphenopoly}/share/hyphenopoly $out
          '')
          (pkgs.runCommand "fonts" { } ''
            mkdir $out
            cd $out

            cp ${pkgs.cm_unicode}/share/fonts/opentype/*.otf .
            cp ${pkgs.iosevka-comfy.comfy}/share/fonts/truetype/*.ttf .
            ls *.otf | ${lib.getExe pkgs.parallel} ${lib.getExe' pkgs.woff2 "woff2_compress"}
            ls *.ttf | ${lib.getExe pkgs.parallel} ${lib.getExe' pkgs.woff2 "woff2_compress"}

            rm *.otf
            rm *.ttf
          '')
        ]
        |> pkgs.linkFarmFromDrvs "blog-thirdparty";

      generator = pkgs.haskellPackages.callCabal2nix "blog-generator" ./generator { };

      watch = pkgs.writeShellScriptBin "blog-watch" ''
        trap 'rm -rf out' EXIT

        mkdir -p out
        ln -sf ${thirdparty} out/thirdparty
        ln -sf /home/ccr/universe/projects/blog/assets/* out/   # FIXME hardcoded path

        ${lib.getExe generator} watch --no-server &
        WATCH_PID=$!

        trap 'kill $WATCH_PID 2>/dev/null; rm -rf out' EXIT

        ${lib.getExe pkgs.serve} out
      '';
    in
    {
      make-shells.blog = {
        inputsFrom = [
          config.make-shells.default.finalPackage
          generator.env
        ];
        buildInputs = with pkgs.haskellPackages; [
          cabal-install
          haskell-language-server
          hlint
          ormolu
        ];
      };

      packages = {
        blog = pkgs.stdenv.mkDerivation {
          name = "blog";
          src = lib.cleanSourceWith {
            src = ./.;
            filter = p: _: !(p |> lib.hasSuffix ".nix");
          };
          buildPhase = ''
            mkdir -p generator/thirdparty/
            ln -sfn ${thirdparty}/* generator/thirdparty/
            export NIX_HASH=$(basename "$out" | cut -d'-' -f1)
            ${lib.getExe generator} build
          '';
          installPhase = ''
            mkdir $out
            cp -r out/* $out
          '';
          dontFixup = true;
          passthru = { inherit thirdparty generator watch; };
        };
      };

      treefmt.programs = {
        ormolu = {
          enable = true;
          includes = [ "${currentDir}/**/*.hs" ];
        };
      };
    };

  configurations.nixos.sisko.module =
    { pkgs, ... }:
    {
      services.nginx.virtualHosts."blog.aciceri.dev" = {
        forceSSL = true;
        enableACME = true;
        locations."/".root = (getSystem pkgs.stdenv.system).packages.blog;
        # Hyphenopoly injects its main script via a blob: URL and compiles WebAssembly;
        # KaTeX and asciinema-player rely on inline style attributes.
        extraConfig = ''
          add_header Strict-Transport-Security "max-age=31536000; includeSubDomains" always;
          add_header Content-Security-Policy "default-src 'self'; script-src 'self' blob: 'wasm-unsafe-eval'; style-src 'self' 'unsafe-inline'; img-src 'self' data:; frame-src https://www.youtube.com; object-src 'none'; base-uri 'self'; form-action 'self'; frame-ancestors 'self'" always;
          add_header X-Frame-Options "SAMEORIGIN" always;
          add_header X-Content-Type-Options "nosniff" always;
          add_header Referrer-Policy "strict-origin-when-cross-origin" always;
          add_header Permissions-Policy "camera=(), microphone=(), geolocation=(), payment=(), usb=(), browsing-topics=()" always;
        '';
      };
    };

  readme.parts.projects = ''
    ### Blog

    My blog is reachable at https://blog.aciceri.dev, the website is continuously deployed.
    The project is stored under [${currentDir}](${currentDir}).

    #### Development

    Enter the project directory and development shell to access Cabal and dependencies.

    To build the served derivation, run:

    ```bash
      nix build .#blog
    ```

    To start a development server with hot reload, run:

    ```bash
      nix run .#blog.watch
    ```
  '';
}
