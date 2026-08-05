# Open WebUI chat frontend for Meridian (Anthropic via Claude Max).
# Public at https://claude.aciceri.dev, protected by Open WebUI's own
# account system (signup disabled — accounts are created by the admin).
{
  configurations.nixos.sisko.module =
    { config, lib, ... }:
    let
      cfg = config.services.open-webui;
    in
    {
      services.open-webui = {
        enable = true;
        host = "127.0.0.1";
        port = 8087;
        environment = {
          # Meridian exposes OpenAI-compatible endpoints; the key is a
          # placeholder (auth happens through the Claude Code SDK).
          OPENAI_API_BASE_URL = "http://127.0.0.1:3456/v1";
          OPENAI_API_KEY = "x";
          ENABLE_OLLAMA_API = "False";

          WEBUI_AUTH = "True";
          # First visit with zero users still shows the admin onboarding;
          # after that nobody can self-register.
          ENABLE_SIGNUP = "False";
          DEFAULT_USER_ROLE = "user";

          # Web search done by Open WebUI itself (results injected into the
          # prompt) — Meridian rejects Anthropic server-side search tools.
          # DuckDuckGo needs no API key.
          ENABLE_WEB_SEARCH = "True";
          WEB_SEARCH_ENGINE = "duckduckgo";
          WEB_SEARCH_RESULT_COUNT = "5";

          # No need to phone home.
          ENABLE_VERSION_UPDATE_CHECK = "False";
          SCARF_NO_ANALYTICS = "True";
          DO_NOT_TRACK = "True";
          ANONYMIZED_TELEMETRY = "False";
        };
      };

      # DynamicUser would put state in /var/lib/private which can't be
      # easily persisted (same workaround as adguard-home).
      systemd.services.open-webui.serviceConfig = {
        DynamicUser = lib.mkForce false;
        User = "open-webui";
        Group = "open-webui";
      };
      users.groups.open-webui = { };
      users.users.open-webui = {
        group = "open-webui";
        isSystemUser = true;
      };

      services.nginx.virtualHosts."claude.aciceri.dev" = {
        enableACME = true;
        forceSSL = true;
        locations."/" = {
          proxyPass = "http://${cfg.host}:${toString cfg.port}";
          proxyWebsockets = true;
          extraConfig = ''
            # SSE streaming from the model.
            proxy_buffering off;
            proxy_read_timeout 600s;
            client_max_body_size 50m;
          '';
        };
      };

      environment.persistence."/persist".directories = [
        "/var/lib/open-webui"
      ];
    };
}
