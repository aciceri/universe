{
  getCurrentDir,
  getSystem,
  lib,
  ...
}:
let
  currentDir = getCurrentDir __curPos;
in
{
  gitignore =
    [
      "backend/dist-newstyle"
      "backend/foodlog.db"
      "frontend/node_modules"
      "frontend/dist"
    ]
    |> map (p: "${currentDir}/${p}");

  perSystem =
    {
      config,
      pkgs,
      lib,
      ...
    }:
    {
      make-shells.foodlog = {
        inputsFrom = [
          config.make-shells.default.finalPackage
          config.packages.foodlog-backend.env
        ];
        buildInputs = with pkgs; [
          haskellPackages.cabal-install
          haskellPackages.haskell-language-server
          haskellPackages.hlint
          haskellPackages.ormolu

          nodejs
          nodePackages.pnpm
          nodePackages.typescript-language-server

          sqlite
          zlib
          pkg-config

          watch
        ];

        env = {
          VITE_API_URL = "http://localhost:8080/api";
          LLM_MODEL = "google/gemini-3-flash-preview";
        };
      };

      packages = {

        foodlog-backend = pkgs.haskell.lib.dontHaddock (pkgs.haskellPackages.callCabal2nix "foodlog-backend" ./backend { });

        foodlog-frontend = pkgs.stdenv.mkDerivation (finalAttrs: {
          pname = "foodlog-frontend";
          version = (lib.importJSON ./frontend/package.json).version;

          src = ./frontend;

          nativeBuildInputs = with pkgs; [
            nodejs
            pnpm
            pnpmConfigHook
          ];

          pnpmDeps = pkgs.fetchPnpmDeps {
            inherit (finalAttrs) pname version src;
            fetcherVersion = 2;
            hash = "sha256-Th5I57TuZrt4IZx2vXHRxtpZ5hrax7W9QU6SPg7fDGM=";
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
        });
      };

      treefmt.programs = {
        ormolu = {
          enable = true;
          includes = [ "${currentDir}/backend/**/*.hs" ];
        };
        prettier = {
          enable = true;
          includes = [
            "${currentDir}/frontend/**/*.{js,jsx,ts,tsx,json,css,scss}"
          ];
        };
      };
    };

  flake.modules.nixos.base =
    {
      config,
      pkgs,
      ...
    }:
    let
      cfg = config.services.foodlog;
      inherit (getSystem pkgs.stdenv.system) packages;
      inherit (packages) foodlog-backend;
      foodlog-frontend = packages.foodlog-frontend.overrideAttrs {
        env.VITE_API_URL = "/api";
      };
    in
    {
      options.services.foodlog = {
        backend = {
          enable = lib.mkEnableOption "Foodlog service";

          port = lib.mkOption {
            type = lib.types.port;
            default = 6547;
          };

          llmModel = lib.mkOption {
            type = lib.types.str;
            default = "google/gemini-3-flash-preview";
          };

          corsOrigins = lib.mkOption {
            type = lib.types.listOf lib.types.str;
          };

          environmentFile = lib.mkOption {
            type = lib.types.path;
          };

          dataDir = lib.mkOption {
            type = lib.types.path;
            default = "/var/lib/foodlog";
          };
        };

        frontend.virtualHost = lib.mkOption {
          type = lib.types.str;
        };
      };

      config = lib.mkIf cfg.backend.enable {
        users.users.foodlog = {
          isSystemUser = true;
          group = "foodlog";
          home = cfg.backend.dataDir;
          createHome = true;
        };

        users.groups.foodlog = { };

        systemd.services.foodlog-backend = {
          description = "Foodlog backend API";
          wantedBy = [ "multi-user.target" ];
          after = [ "network.target" ];

          environment = {
            PORT = toString cfg.backend.port;
            LLM_MODEL = cfg.backend.llmModel;
            DATABASE_PATH = "${cfg.backend.dataDir}/foodlog.db";
            CORS_ORIGINS = lib.concatStringsSep "," cfg.backend.corsOrigins;
          };

          serviceConfig = {
            ExecStart = lib.getExe foodlog-backend;
            User = "foodlog";
            Group = "foodlog";
            StateDirectory = "foodlog";
            WorkingDirectory = cfg.backend.dataDir;
            EnvironmentFile = lib.mkIf (cfg.backend.environmentFile != null) cfg.backend.environmentFile;

            # Security hardening - TEMPORARILY DISABLED FOR DEBUGGING
            # NoNewPrivileges = true;
            # PrivateTmp = true;
            # ProtectSystem = "strict";
            # ProtectHome = true;
            # RestrictAddressFamilies = [ "AF_INET" "AF_INET6" ];
          };
        };

        services.nginx.virtualHosts.${cfg.frontend.virtualHost} = {
          root = foodlog-frontend;
          locations."/" = {
            tryFiles = "$uri $uri/ /index.html";
          };
          locations."/api/" = {
            proxyPass = "http://127.0.0.1:${toString cfg.backend.port}/api/";
            extraConfig = ''
              proxy_set_header Host $host;
              proxy_set_header X-Real-IP $remote_addr;
              proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
              proxy_set_header X-Forwarded-Proto $scheme;
            '';
          };
        };
      };
    };

  readme.parts.projects = ''
    ### Foodlog

    Vibe coded AI-powered progressive web app for tracking my food intake.
  '';
}
