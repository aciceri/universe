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
  codeConfigFor =
    config:
    let
      stylixColors = config.lib.stylix.colors.withHashtag;
    in
    {
      approval_policy = "untrusted";
      sandbox_mode = "danger-full-access";
      tui = {
        theme = {
          colors = with stylixColors; {
            primary = base0D;
            secondary = base0E;
            background = base00;
            foreground = base05;
            border = base01;
            border_focused = base0D;
            selection = base02;
            cursor = base05;
            success = base0B;
            warning = base0A;
            error = base08;
            info = base0C;
            text = base05;
            text_dim = base04;
            text_bright = base06;
            keyword = base0E;
            string = base0B;
            comment = base03;
            function = base09;
            spinner = base0E;
            progress = base0D;
          };
          name = "custom";
        };
      };
    };
in
{ lib, ... }:
{
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
          mcpServers = {
            openmemory = {
              type = "stdio";
              command = "docker";
              args = [
                "run"
                "-i"
                "-v"
                "claude-memory:/app/dist"
                "--rm"
                "mcp/memory"
              ];
            };
          };
        };

        codex = {
          enable = true;
          custom-instructions = instructions;
          settings = {
            sandbox_mode = "danger-full-access";
            approval_policy = "untrusted";
          };
        };

        gemini-cli = {
          enable = true;
          context.GEMINI = instructions;
        };

        opencode = {
          enable = true;
          settings = {
            theme = lib.mkForce "catppuccin";
            formatter = false;
            plugin = [
              "opencode-anthropic-auth@latest"
              "opencode-openai-codex-auth@4.4.0"
            ];
            mcp = {
              playwright = {
                command = [ (lib.getExe pkgs.playwright-mcp) ];
                type = "local";
                enabled = false;
              };
              github = {
                command = [
                  (lib.getExe pkgs.github-mcp-server)
                  "stdio"
                ];
                type = "local";
                enabled = false;
              };
            };
            provider.openai = {
              options = {
                reasoningEffort = "medium";
                reasoningSummary = "auto";
                textVerbosity = "medium";
                include = [ "reasoning.encrypted_content" ];
                store = false;
              };
              models = {
                "gpt-5.2" = {
                  name = "GPT 5.2 (OAuth)";
                  limit = {
                    context = 272000;
                    output = 128000;
                  };
                  modalities = {
                    input = [
                      "text"
                      "image"
                    ];
                    output = [ "text" ];
                  };
                  variants = {
                    none = {
                      reasoningEffort = "none";
                      reasoningSummary = "auto";
                      textVerbosity = "medium";
                    };
                    low = {
                      reasoningEffort = "low";
                      reasoningSummary = "auto";
                      textVerbosity = "medium";
                    };
                    medium = {
                      reasoningEffort = "medium";
                      reasoningSummary = "auto";
                      textVerbosity = "medium";
                    };
                    high = {
                      reasoningEffort = "high";
                      reasoningSummary = "detailed";
                      textVerbosity = "medium";
                    };
                    xhigh = {
                      reasoningEffort = "xhigh";
                      reasoningSummary = "detailed";
                      textVerbosity = "medium";
                    };
                  };
                };
                "gpt-5.2-codex" = {
                  name = "GPT 5.2 Codex (OAuth)";
                  limit = {
                    context = 272000;
                    output = 128000;
                  };
                  modalities = {
                    input = [
                      "text"
                      "image"
                    ];
                    output = [ "text" ];
                  };
                  variants = {
                    low = {
                      reasoningEffort = "low";
                      reasoningSummary = "auto";
                      textVerbosity = "medium";
                    };
                    medium = {
                      reasoningEffort = "medium";
                      reasoningSummary = "auto";
                      textVerbosity = "medium";
                    };
                    high = {
                      reasoningEffort = "high";
                      reasoningSummary = "detailed";
                      textVerbosity = "medium";
                    };
                    xhigh = {
                      reasoningEffort = "xhigh";
                      reasoningSummary = "detailed";
                      textVerbosity = "medium";
                    };
                  };
                };
                "gpt-5.1-codex-max" = {
                  name = "GPT 5.1 Codex Max (OAuth)";
                  limit = {
                    context = 272000;
                    output = 128000;
                  };
                  modalities = {
                    input = [
                      "text"
                      "image"
                    ];
                    output = [ "text" ];
                  };
                  variants = {
                    low = {
                      reasoningEffort = "low";
                      reasoningSummary = "detailed";
                      textVerbosity = "medium";
                    };
                    medium = {
                      reasoningEffort = "medium";
                      reasoningSummary = "detailed";
                      textVerbosity = "medium";
                    };
                    high = {
                      reasoningEffort = "high";
                      reasoningSummary = "detailed";
                      textVerbosity = "medium";
                    };
                    xhigh = {
                      reasoningEffort = "xhigh";
                      reasoningSummary = "detailed";
                      textVerbosity = "medium";
                    };
                  };
                };
                "gpt-5.1-codex" = {
                  name = "GPT 5.1 Codex (OAuth)";
                  limit = {
                    context = 272000;
                    output = 128000;
                  };
                  modalities = {
                    input = [
                      "text"
                      "image"
                    ];
                    output = [ "text" ];
                  };
                  variants = {
                    low = {
                      reasoningEffort = "low";
                      reasoningSummary = "auto";
                      textVerbosity = "medium";
                    };
                    medium = {
                      reasoningEffort = "medium";
                      reasoningSummary = "auto";
                      textVerbosity = "medium";
                    };
                    high = {
                      reasoningEffort = "high";
                      reasoningSummary = "detailed";
                      textVerbosity = "medium";
                    };
                  };
                };
                "gpt-5.1-codex-mini" = {
                  name = "GPT 5.1 Codex Mini (OAuth)";
                  limit = {
                    context = 272000;
                    output = 128000;
                  };
                  modalities = {
                    input = [
                      "text"
                      "image"
                    ];
                    output = [ "text" ];
                  };
                  variants = {
                    medium = {
                      reasoningEffort = "medium";
                      reasoningSummary = "auto";
                      textVerbosity = "medium";
                    };
                    high = {
                      reasoningEffort = "high";
                      reasoningSummary = "detailed";
                      textVerbosity = "medium";
                    };
                  };
                };
                "gpt-5.1" = {
                  name = "GPT 5.1 (OAuth)";
                  limit = {
                    context = 272000;
                    output = 128000;
                  };
                  modalities = {
                    input = [
                      "text"
                      "image"
                    ];
                    output = [ "text" ];
                  };
                  variants = {
                    none = {
                      reasoningEffort = "none";
                      reasoningSummary = "auto";
                      textVerbosity = "medium";
                    };
                    low = {
                      reasoningEffort = "low";
                      reasoningSummary = "auto";
                      textVerbosity = "low";
                    };
                    medium = {
                      reasoningEffort = "medium";
                      reasoningSummary = "auto";
                      textVerbosity = "medium";
                    };
                    high = {
                      reasoningEffort = "high";
                      reasoningSummary = "detailed";
                      textVerbosity = "high";
                    };
                  };
                };
              };
            };
          };
          rules = instructions;
        };
      };

      home.file.".code/config.toml".source = (pkgs.formats.toml { }).generate "codex-config" (codeConfigFor config);

      home.packages = with pkgs; [
        code
        cursor-cli
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
