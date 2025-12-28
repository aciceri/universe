{ getSystem, getCurrentDir, ... }:
let
  currentDir = getCurrentDir __curPos;
in
{
  gitignore =
    [
      "node_modules"
      "dist"
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
      make-shells.talks-agenix-shell = {
        inputsFrom = [
          config.make-shells.default.finalPackage
        ];
        buildInputs = config.packages.agenix-shell-talk.nativeBuildInputs;
      };

      packages.agenix-shell-talk = pkgs.stdenv.mkDerivation (finalAttrs: {
        pname = "agenix-shell-talk";
        version = "1.0.0";

        src = lib.cleanSourceWith {
          src = ./.;
          filter = path: _: !(lib.hasSuffix ".nix" path);
        };

        nativeBuildInputs = with pkgs; [
          nodejs
          pnpm
          pnpmConfigHook
        ];

        pnpmDeps = pkgs.fetchPnpmDeps {
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
          description = "Presentation about agenix-shell for a 5-minute flash talk at NixCon 2025";
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
      services.nginx.virtualHosts."agenix-shell.talks.aciceri.dev" = {
        forceSSL = true;
        enableACME = true;
        locations."/".root = (getSystem pkgs.stdenv.system).packages.agenix-shell-talk;
      };
    };

  readme.parts.projects = ''
    ### `agenix-shell` talk

    A presentation about [agenix-shell](https://github.com/aciceri/agenix-shell) for a 5-minute flash talk at NixCon 2025.
    [Here](https://www.youtube.com/watch?v=pE3wha4jlos) the recording.

    #### Running the Slides

    ```bash
    pnpm install  # Install dependencies
    pnpm dev      # Start development server
    ```

    This will start a development server for the presentation slides.

    #### Building

    ```bash
    pnpm build                        # Build for production
    nix build .#agenix-shell-talk     # Build presentation as a completely self-contained derivation
    ```
  '';
}
