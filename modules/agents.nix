let
  instructions = ''
    # Personal Preferences

    ## Language & Communication
    - Always produce code and comments in English, regardless of my query language
    - Reply to me in the same language I use (Italian, English, etc.)
    - Keep technical terms in English even in non-English responses

    ## MCP Tools (omp)
    - MCP tools ARE available even when they don't appear among native tools:
      omp mounts them as `xd://mcp__<server>_<tool>` devices (see "MCP Tool
      Routes" in the system prompt). Invoke one by writing its JSON args to
      that path.
    - Never claim an MCP server (grain, linear, slack, gcal, emacs, ...) is
      unavailable without first checking the routes or reading `xd://`.

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

  llmAgentsOverlay =
    { inputs }:
    {
      nixpkgs.overlays = [
        inputs.llm-agents.overlays.shared-nixpkgs
        # collab-autostart: upstream candidate (oh-my-pi#6171);
        # collab-linkfile: local only, drop once oh-my-pi#6354 lands.
        (_final: prev: {
          llm-agents = prev.llm-agents // {
            omp = prev.llm-agents.omp.overrideAttrs (old: {
              patches = (old.patches or [ ]) ++ [
                ./patches/omp-collab-autostart.patch
                ./patches/omp-collab-linkfile.patch
              ];
            });
          };
        })
      ];
    };
in
{
  config,
  inputs,
  ...
}:
{
  # Provide the llm-agents overlay (and `omp`) on every host that wants
  # claude-code/opencode.
  flake.modules.nixos.claude-code-overlay = llmAgentsOverlay { inherit inputs; };
  flake.modules.darwin.claude-code-overlay = llmAgentsOverlay { inherit inputs; };

  # Cross-platform claude-code config + opencode (omp).
  flake.modules.homeManager.claude-code =
    { pkgs, lib, ... }:
    {
      programs.claude-code = {
        enable = true;
        context = instructions;
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

      home.packages = with pkgs; [
        llm-agents.omp
        llm-agents.hermes-agent
        llm-agents.hermes-desktop
        llm-agents.zeroclaw
      ];
    };

  # OTEL telemetry endpoints — point to sisko inside WireGuard. Useful only
  # for hosts that can reach the WG network.
  flake.modules.homeManager.claude-code-otel =
    { osConfig, config, ... }:
    {
      programs.claude-code.settings.env = {
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

  # Inject OPENROUTER_API_KEY into nushell from an agenix secret. Requires
  # the host to declare `secrets.openrouter_api_key.owner = "<user>";`.
  flake.modules.homeManager.claude-code-openrouter-env =
    { osConfig, ... }:
    {
      programs.nushell.extraConfig = ''
        $env.OPENROUTER_API_KEY = (open ${osConfig.age.secrets.openrouter_api_key.path} | str trim)
      '';
    };

  # NixOS workstations get the full bundle (overlay + secret + HM extras).
  flake.modules.nixos.workstation = {
    imports = with config.flake.modules.nixos; [ claude-code-overlay ];
    secrets.openrouter_api_key.owner = "ccr";
  };

  flake.modules.homeManager.workstation.imports = with config.flake.modules.homeManager; [
    claude-code
    claude-code-otel
    claude-code-openrouter-env
  ];

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

      systemd.services.meridian = {
        description = "Meridian - local Anthropic API powered by Claude Max";
        after = [ "network-online.target" ];
        wants = [ "network-online.target" ];
        wantedBy = [ "multi-user.target" ];
        path = [
          pkgs.claude-code
          pkgs.which
        ];
        environment = {
          MERIDIAN_HOST = "127.0.0.1";
          MERIDIAN_PORT = "3456";
          MERIDIAN_CLAUDE_PATH = lib.getExe pkgs.claude-code;
        };
        serviceConfig = {
          # Long-lived OAuth token from `claude setup-token`, stored manually:
          #   echo "CLAUDE_CODE_OAUTH_TOKEN=sk-ant-oat01-..." > /var/lib/claude-heartbeat/oauth-token.env
          # The `-` prefix makes it optional so the service still starts without it.
          EnvironmentFile = [ "-/var/lib/claude-heartbeat/oauth-token.env" ];
          Type = "simple";
          User = "claude-heartbeat";
          WorkingDirectory = config.users.users.claude-heartbeat.home;
          ExecStart = lib.getExe pkgs.meridian;
          Restart = "always";
          RestartSec = 5;
        };
      };

      services.nginx.virtualHosts."claude.sisko.wg.aciceri.dev" = {
        forceSSL = true;
        useACMEHost = "aciceri.dev";
        serverAliases = [ "claude.sisko.zt.aciceri.dev" ];
        locations."/" = {
          proxyPass = "http://127.0.0.1:3456";
          proxyWebsockets = true;
          # Streaming (SSE) + large multimodal uploads.
          extraConfig = ''
            proxy_buffering off;
            proxy_request_buffering off;
            proxy_cache off;
            proxy_read_timeout 600s;
            proxy_send_timeout 600s;
            client_max_body_size 100m;
          '';
        };
        extraConfig = ''
          allow 10.100.0.0/24;
          allow 10.100.1.0/24;
          allow 10.88.0.0/16;
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
      environment.persistence."/persist".directories = [
        "/var/lib/claude-heartbeat"
      ];
    };
}
