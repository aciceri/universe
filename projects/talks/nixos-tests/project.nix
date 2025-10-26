{ getSystem, getCurrentDir, ... }:
let
  currentDir = getCurrentDir __curPos;
in
{
  gitignore =
    [
      "node_modules"
      "dist"
      ".vite"
    ]
    |> builtins.map (path: "${currentDir}/**/${path}");

  perSystem =
    {
      pkgs,
      config,
      lib,
      ...
    }:
    {
      make-shells.talks-nixos-tests = {
        inputsFrom = [
          config.make-shells.default.finalPackage
        ];
        buildInputs = config.packages.nixos-tests-talk.nativeBuildInputs;
      };

      packages.nixos-tests-talk = pkgs.stdenv.mkDerivation (finalAttrs: {
        pname = "nixos-tests-talk";
        version = "1.0.0";

        src = lib.cleanSourceWith {
          src = ./.;
          filter = path: _: !(lib.hasSuffix ".nix" path);
        };

        nativeBuildInputs = with pkgs; [
          nodejs
          pnpm.configHook
        ];

        pnpmDeps = pkgs.pnpm.fetchDeps {
          inherit (finalAttrs) pname version src;
          fetcherVersion = 2;
          hash = "sha256-i3vEHiH69Zp/Dwq6EZ2GWnigyjWVVBD+71qQmeR9s6k=";
        };

        buildPhase = ''
          runHook preBuild
          pnpm build
          runHook postBuild
        '';

        installPhase = ''
          runHook preInstall
          cp -r dist $out
          runHook postInstall
        '';

        meta = {
          description = "Presentation about the NixOS tests framework for the 2025 Milan Linux Day";
          license = lib.licenses.mit;
          maintainers = with lib.maintainers; [ aciceri ];
        };
      });

      treefmt.programs = {
        prettier = {
          enable = true;
          includes =
            [
              "json"
              "yaml"
              "js"
              "html"
            ]
            |> lib.map (ext: "${currentDir}/**/*.${ext}");
        };
      };
    };

  configurations.nixos.sisko.module =
    { pkgs, ... }:
    {
      services.nginx.virtualHosts."nixos-tests.talks.aciceri.dev" = {
        forceSSL = true;
        enableACME = true;
        locations."/".root = (getSystem pkgs.stdenv.system).packages.nixos-tests-talk;
      };
    };

  readme.parts.projects = ''
    ### NixOS testing framework talk

    Talk about the NixOS tests framework for 2025 Milan Linux Day.

    #### Running the Slides

    ```bash
    pnpm install  # Install dependencies
    pnpm dev      # Start development server
    ```

    This will start a development server for the presentation slides.

    #### Building

    ```bash
    pnpm build                        # Build for production
    nix build .#nixos-tests-talk      # Build presentation as a completely self-contained derivation
    ```
  '';
}
