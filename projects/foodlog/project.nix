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
      "backend-old/dist-newstyle"
      "backend-old/foodlog.db"
      "backend/node_modules"
      "backend/dist"
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
        ];
        buildInputs = with pkgs; [
          bun

          nodejs
          pnpm
          typescript-language-server

          sqlite
        ];

        env = {
          VITE_API_URL = "http://localhost:8080/api";
        };
      };

      packages = {
        foodlog-backend = pkgs.stdenv.mkDerivation (finalAttrs: {
          pname = "foodlog-backend";
          version = (lib.importJSON ./backend/package.json).version;

          src = ./backend;

          nativeBuildInputs = with pkgs; [
            nodejs
            pnpm
            pnpmConfigHook
            makeBinaryWrapper
          ];

          pnpmDeps = pkgs.fetchPnpmDeps {
            inherit (finalAttrs) pname version src;
            fetcherVersion = 2;
            hash = "sha256-gL4PZrLwMgArh97C8Op8Qh8WwF+iuYruCIRWja2tjw4=";
          };

          # No build step needed — Bun runs TypeScript directly
          dontBuild = true;

          installPhase = ''
            runHook preInstall
            mkdir -p $out/lib/foodlog-backend
            cp -r src $out/lib/foodlog-backend/
            cp -r node_modules $out/lib/foodlog-backend/
            cp package.json $out/lib/foodlog-backend/

            mkdir -p $out/bin
            makeBinaryWrapper ${pkgs.bun}/bin/bun $out/bin/foodlog-backend \
              --add-flags "run $out/lib/foodlog-backend/src/index.ts"
            runHook postInstall
          '';

          meta.mainProgram = "foodlog-backend";
        });

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
            hash = "sha256-T+h8RXbcri2JDpAEp07XuPpohg7byHqWvY4LM/zQNqE=";
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
        prettier = {
          enable = true;
          includes = [
            "${currentDir}/frontend/**/*.{js,jsx,ts,tsx,json,css,scss}"
            "${currentDir}/backend/src/**/*.{ts,js,json}"
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

          model = lib.mkOption {
            type = lib.types.str;
            default = "opus";
            description = "Claude model to use (opus, sonnet, haiku)";
          };

          corsOrigins = lib.mkOption {
            type = lib.types.listOf lib.types.str;
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
          packages = [ pkgs.claude-code ];
        };

        users.groups.foodlog = { };

        systemd.tmpfiles.rules = [
          "d ${cfg.backend.dataDir} 0750 foodlog foodlog -"
        ];

        # To authenticate the Claude Agent SDK on the server, run:
        #   sudo -u foodlog HOME=/var/lib/foodlog /etc/profiles/per-user/foodlog/bin/claude login
        # This stores credentials in /var/lib/foodlog/.claude/
        # The systemd service sets HOME to dataDir so the SDK finds them.

        systemd.services.foodlog-backend = {
          description = "Foodlog backend API";
          wantedBy = [ "multi-user.target" ];
          after = [ "network.target" ];

          # claude-code in PATH is needed by the Claude Agent SDK at runtime
          # (it spawns `claude` as a subprocess).
          # bun is needed because the SDK detects the Bun runtime and spawns
          # child processes using `bun` from PATH.
          path = [
            pkgs.claude-code
            pkgs.bun
          ];

          environment = {
            PORT = toString cfg.backend.port;
            CLAUDE_MODEL = cfg.backend.model;
            DATABASE_PATH = "${cfg.backend.dataDir}/foodlog.db";
            CORS_ORIGINS = lib.concatStringsSep "," cfg.backend.corsOrigins;
            HOME = cfg.backend.dataDir;
          };

          serviceConfig = {
            ExecStart = lib.getExe foodlog-backend;
            User = "foodlog";
            Group = "foodlog";
            StateDirectory = "foodlog";
            StateDirectoryMode = "0750";
            WorkingDirectory = cfg.backend.dataDir;
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

              # SSE support
              proxy_buffering off;
              proxy_cache off;
              proxy_read_timeout 300s;
              proxy_set_header X-Accel-Buffering no;
              chunked_transfer_encoding on;
            '';
          };
        };
      };
    };

  readme.parts.projects = ''
    ### Foodlog

    AI-powered progressive web app for tracking food intake, powered by Claude Agent SDK with MCP tools.
  '';
}
