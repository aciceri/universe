{ config, ... }:
{
  gitignore =
    [
      "_cache"
      "out"
      "generator/dist-newstyle"
      "generator/thirdparty"
    ]
    |> builtins.map (path: "projects/blog/${path}");

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
            cp -r ${pkgs.nodePackages.katex}/lib/node_modules/katex/dist $out
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

      generator = (pkgs.haskellPackages.callCabal2nix "blog-generator" ./generator { });

      watch = pkgs.writeShellScriptBin "blog-watch" ''
        trap 'rm -rf out' EXIT

        mkdir -p out
        ln -sf ${thirdparty} out/thirdparty
        ln -sf /home/ccr/universe/projects/blog/assets/* out/

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
            filter = path: _: !(path |> lib.hasSuffix ".nix");
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
          includes = [ "projects/blog/**/*.hs" ];
        };
      };
    };

  configurations.nixos.sisko.module =
    { pkgs, ... }:
    {
      services.nginx.virtualHosts."blog.aciceri.dev" = {
        forceSSL = true;
        enableACME = true;
        locations."/".root = config.flake.packages.${pkgs.stdenv.system}.blog;
      };
    };
}
