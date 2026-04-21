let
  instructions = ''
    # Personal Preferences

    ## Language & Communication
    - Always produce code and comments in English, regardless of my query language
    - Reply to me in the same language I use (Italian, English, etc.)
    - Keep technical terms in English even in non-English responses

    ## Environment: NixOS
    - System: NixOS with Home Manager and Flakes
    - Never attempt imperative installations (no apt, dnf, curl | bash, etc.)
    - If a program is missing:
      1. First check if it's already in PATH
      2. For project-specific tools: add to flake.nix devShell
      3. For one-off usage: use `yes | , <command>` (comma tool via nix-index)

    ## Development Workflow
    - Check for flake.nix before suggesting installations
    - Prefer `nix develop` shells over global installations
    - Test commands before committing
    - Use `direnv` integration when available (auto-load devShell)
    - Feel free to use `gh` for GitHub operations

    ## Git Usage
    - Do NOT create commits unless explicitly requested
    - Do NOT use git add, git commit, or other git commands unless absolutely necessary for the task
    - In general, the user prefers to handle git operations manually
    - Only interact with git when it's essential to complete a specific task

    ## Code Style
    - Prefer clarity over cleverness
    - Meaningful variable names (no single letters except loops)
    - Comments for "why", not "what"
    - Break complex operations into readable steps
  '';
in
{ inputs, lib, ... }:
{
  flake.modules.nixos.workstation = {
    nixpkgs.overlays = [ inputs.llm-agents.overlays.shared-nixpkgs ];
  };

  flake.modules.homeManager.workstation =
    {
      osConfig,
      pkgs,
      config,
      ...
    }:
    {
      programs = {
        claude-code = {
          enable = true;
          memory.text = instructions;
          settings = {
            env = {
              CLAUDE_CODE_ENABLE_TELEMETRY = "1";
              OTEL_METRICS_EXPORTER = "otlp";
              OTEL_LOGS_EXPORTER = "otlp";
              OTEL_EXPORTER_OTLP_METRICS_ENDPOINT = "http://sisko.wg.aciceri.dev:4317";
              OTEL_EXPORTER_OTLP_LOGS_ENDPOINT = "http://sisko.wg.aciceri.dev:3100/otlp/v1/logs";
              OTEL_EXPORTER_OTLP_METRICS_PROTOCOL = "grpc";
              OTEL_EXPORTER_OTLP_LOGS_PROTOCOL = "http/protobuf";
              OTEL_RESOURCE_ATTRIBUTES = "host.name=${osConfig.networking.hostName},user.name=${config.home.username}";
              OTEL_LOG_USER_PROMPTS = "1";
              OTEL_METRIC_EXPORT_INTERVAL = "60000";
              OTEL_LOGS_EXPORT_INTERVAL = "5000";
            };
          };
          mcpServers =
            let
              npxWithNode = pkgs.writeShellScript "npx-with-node" ''
                export PATH="${lib.makeBinPath [ pkgs.nodejs ]}:$PATH"
                exec ${lib.getExe' pkgs.nodejs "npx"} "$@"
              '';
            in
            {
              linear = {
                command = "${npxWithNode}";
                args = [
                  "-y"
                  "mcp-remote"
                  "https://mcp.linear.app/mcp"
                ];
                disabled = true;
              };
              home-assistant = {
                command =
                  let
                    wrapper = pkgs.writeShellScript "ha-mcp" ''
                      export PATH="${lib.makeBinPath [ pkgs.nodejs ]}:$PATH"
                      TOKEN=$(cat ~/.config/home-assistant/mcp-token)
                      exec ${lib.getExe' pkgs.nodejs "npx"} -y mcp-remote \
                        https://home.aciceri.dev/api/mcp \
                        --header "Authorization: Bearer $TOKEN"
                    '';
                  in
                  "${wrapper}";
                disabled = true;
              };
            };
        };
      };

      home.packages = with pkgs; [
        llm-agents.omp
      ];
    };

  configurations.nixos.sisko.module =
    {
      config,
      pkgs,
      lib,
      ...
    }:
    {
      users.groups.claude-heartbeat = { };
      users.users.claude-heartbeat = {
        group = "claude-heartbeat";
        home = "/var/lib/claude-heartbeat";
        isSystemUser = true;
        createHome = true;
      };

      systemd.services.claude-proxy = {
        description = "Proxy bridging Anthropic API to Claude Max via Claude Agent SDK";
        after = [ "network-online.target" ];
        wants = [ "network-online.target" ];
        wantedBy = [ "multi-user.target" ];
        path = [
          pkgs.claude-code
          pkgs.which
        ];
        serviceConfig = {
          Type = "simple";
          User = "claude-heartbeat";
          WorkingDirectory = config.users.users.claude-heartbeat.home;
          ExecStart = lib.getExe pkgs.opencode-claude-max-proxy;
          Restart = "always";
          RestartSec = 5;
        };
      };

      services.nginx.virtualHosts."claude.sisko.wg.aciceri.dev" = {
        forceSSL = true;
        useACMEHost = "aciceri.dev";
        locations."/" = {
          proxyPass = "http://localhost:3456";
          proxyWebsockets = true;
          extraConfig = ''
            proxy_buffering off;
            proxy_cache off;
            proxy_read_timeout 300s;
          '';
        };
        serverAliases = [ "claude.sisko.zt.aciceri.dev" ];
        extraConfig = ''
          allow 10.100.0.0/24;
          allow 10.100.1.0/24;
          allow 127.0.0.1;
          deny all;
        '';
      };

      systemd.services.claude-heartbeat = {
        description = "Automatically start sessions at strategic hours";
        serviceConfig = {
          Type = "oneshot";
          User = "claude-heartbeat";
          WorkingDirectory = config.users.users.claude-heartbeat.home;
          RestrictAddressFamilies = "AF_INET AF_INET6";
          ProtectSystem = "full";
          PrivateTmp = true;
          NoNewPrivileges = true;
          StandardOutput = "journal";
          StandardError = "journal";
        };
        script = ''
          ${lib.getExe pkgs.opencode} run "Lol"
        '';
      };

      systemd.timers.claude-heartbeat = {
        description = "Timer for claude-heartbeat service";
        wantedBy = [ "timers.target" ];
        timerConfig = {
          OnCalendar = [
            "07:00"
            "12:05"
            "17:10"
            "21:15"
          ];
          Persistent = true;
          Unit = "claude-heartbeat.service";
        };
      };

      services.litellm = {
        enable = true;
        port = 41929;
        settings.model_list = [
          {
            model_name = "claude-opus";
            litellm_params = {
              model = "anthropic/claude-opus";
              api_base = "https://claude.sisko.wg.aciceri.dev";
              api_key = "dummy";
            };
          }
          {
            model_name = "claude-sonnet";
            litellm_params = {
              model = "anthropic/claude-sonnet";
              api_base = "https://claude.sisko.wg.aciceri.dev";
              api_key = "dummy";
            };
          }
        ];
      };

      services.nginx.virtualHosts."litellm.sisko.wg.aciceri.dev" = {
        forceSSL = true;
        useACMEHost = "aciceri.dev";
        locations."/" = {
          proxyPass = "http://127.0.0.1:41929";
          proxyWebsockets = true;
          extraConfig = ''
            proxy_buffering off;
            proxy_cache off;
            proxy_read_timeout 300s;
          '';
        };
        extraConfig = ''
          allow 10.100.0.0/24;
          allow 10.100.1.0/24;
          allow 127.0.0.1;
          deny all;
        '';
      };

      environment.persistence."/persist".directories = [
        "/var/lib/claude-heartbeat"
      ];
    };
}
