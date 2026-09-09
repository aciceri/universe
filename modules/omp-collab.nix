# Self-hosted omp collab stack: relay on sisko, omp-session-gateway on pike.
#
# The relay only shuttles E2E-encrypted frames between an omp host and a
# browser guest; it never learns a room key. The gateway is the phone-facing
# half: patched omp auto-hosts a room per interactive session and publishes it
# over an authenticated unix socket, the gateway lists those sessions and hands
# out view/control capabilities on tap.
#
# Path from the phone: https://omp.sisko.wg.aciceri.dev (TLS + WireGuard ACL on
# sisko) -> pike:80 -> pike's own nginx -> 127.0.0.1:4317. The last hop matters:
# the gateway serves only loopback peers, so the reverse proxy has to be local
# to it.
#
# Upstream supports exactly one remote path — Tailscale Serve, which strips and
# re-adds the identity header it authenticates with. WireGuard has no such
# notion, so pike's nginx asserts the identity itself and the config declares
# `trustIdentityWithoutTailnetDevice`. The guard is then purely topological:
# reaching that vhost requires the WireGuard tunnel, exactly like the dashboard
# this replaced, which handed room secrets to every peer on the subnet.
#
# Clients:
#   omp config set collab.relayUrl wss://collab.sisko.wg.aciceri.dev
#   omp config set collab.webUrl https://my.omp.sh
#   omp config set collab.autoStart control   # publish to the gateway
#   omp config set collab.registryEndpoint auto
{
  configurations.nixos.pike.module =
    {
      pkgs,
      lib,
      ...
    }:
    let
      port = 4317;
      publicOrigin = "https://omp.sisko.wg.aciceri.dev";
      # Any stable string works: nothing verifies it against a real tailnet
      # account, it only has to match what nginx injects below.
      login = "ccr@aciceri.dev";

      gatewayConfig = (pkgs.formats.json { }).generate "omp-session-gateway-config.json" {
        http = {
          hostname = "127.0.0.1";
          inherit port;
          inherit publicOrigin;
        };
        auth = {
          mode = "tailscale-serve";
          allowedLogins = [ login ];
          trustIdentityWithoutTailnetDevice = true;
        };
        registry = {
          heartbeatSeconds = 10;
          ttlSeconds = 35;
          maxPublishers = 100;
          maxSessions = 100;
        };
      };

      # The daemon refuses a symlinked or group-readable config, so the store
      # copy is installed as a private regular file instead of being linked in
      # by home-manager. The publisher token next to it is created by the
      # daemon on first start and is deliberately left alone: rotating it would
      # orphan the omp processes already holding it.
      installConfig = pkgs.writeShellScript "omp-session-gateway-config" ''
        dir="''${XDG_CONFIG_HOME:-$HOME/.config}/omp-session-gateway"
        ${pkgs.coreutils}/bin/install -d -m 0700 "$dir"
        ${pkgs.coreutils}/bin/install -m 0600 ${gatewayConfig} "$dir/config.json"
      '';
    in
    {
      # WireGuard peers only: the gateway hands out collab room secrets.
      networking.firewall.interfaces."wg-universe".allowedTCPPorts = [ 80 ];

      services.nginx = {
        enable = true;
        virtualHosts."omp.sisko.wg.aciceri.dev" = {
          default = true;
          locations."/" = {
            proxyPass = "http://127.0.0.1:${toString port}";
            extraConfig = ''
              # Authenticates the request for the gateway: it trusts this header
              # from a loopback peer, so it must be set here and never forwarded
              # from the client.
              proxy_set_header Tailscale-User-Login ${login};

              # /api/v1/events is a long-lived SSE stream with a 5s keepalive.
              proxy_buffering off;
              proxy_read_timeout 1h;
              proxy_send_timeout 1h;
            '';
          };
        };
      };

      home-manager.users.ccr =
        { ... }:
        {
          home.packages = [ pkgs.omp-session-gateway ];

          # A user service, not a system one: the registry socket lives in
          # $XDG_RUNTIME_DIR, and the omp processes that publish to it derive
          # that same path from their own session.
          systemd.user.services.omp-session-gateway = {
            Unit = {
              Description = "omp session gateway (live collab session directory)";
              After = [ "network-online.target" ];
            };
            Service = {
              ExecStartPre = toString installConfig;
              ExecStart = lib.getExe' pkgs.omp-session-gateway "omp-gatewayd";
              # 0700 because the daemon asserts its runtime directory is
              # unreadable by anyone else before binding the socket.
              RuntimeDirectory = "omp-session-gateway";
              RuntimeDirectoryMode = "0700";
              Restart = "on-failure";
              RestartSec = 5;
            };
            Install.WantedBy = [ "default.target" ];
          };
        };
    };

  configurations.nixos.sisko.module =
    { pkgs, lib, ... }:
    {
      systemd.services.omp-collab-relay = {
        description = "omp collab relay (E2E-encrypted session sharing)";
        wantedBy = [ "multi-user.target" ];
        after = [ "network.target" ];
        serviceConfig = {
          ExecStart = lib.getExe pkgs.omp-collab-relay;
          DynamicUser = true;
          Restart = "on-failure";
        };
      };

      services.nginx.virtualHosts."collab.sisko.wg.aciceri.dev" = {
        forceSSL = true;
        useACMEHost = "aciceri.dev";
        locations."/" = {
          proxyPass = "http://127.0.0.1:7475";
          proxyWebsockets = true;
          # Don't let the default 60s proxy timeout cut idle websockets.
          extraConfig = ''
            proxy_read_timeout 1h;
            proxy_send_timeout 1h;
          '';
        };
        extraConfig = ''
          allow 10.100.0.0/24;
          allow 10.100.1.0/24;
          allow 127.0.0.1;
          deny all;
        '';
      };

      services.nginx.virtualHosts."omp.sisko.wg.aciceri.dev" = {
        forceSSL = true;
        useACMEHost = "aciceri.dev";
        # Gateway runs on pike, reached over WireGuard by IP.
        locations."/" = {
          proxyPass = "http://10.100.0.8:80";
          extraConfig = ''
            # SSE session stream: no buffering, no 60s cutoff.
            proxy_buffering off;
            proxy_read_timeout 1h;
            proxy_send_timeout 1h;

            # Only pike's nginx may assert an identity to the gateway.
            proxy_set_header Tailscale-User-Login "";
          '';
        };
        extraConfig = ''
          allow 10.100.0.0/24;
          allow 10.100.1.0/24;
          allow 127.0.0.1;
          deny all;
        '';
      };
    };
}
