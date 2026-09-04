# ZeroClaw personal-assistant daemon, always-on on sisko (unlike the
# workstation CLI, which only runs while a laptop is open), with its
# gateway/dashboard reachable over WireGuard at
# https://zeroclaw.sisko.wg.aciceri.dev.
#
# Deliberately NOT using zeroclaw's own upstream `services.zeroclaw.instances`
# NixOS module (github:zeroclaw-labs/zeroclaw, nix/module.nix): that module
# renders a Nix `settings` attrset into config.toml at build time. Doing that
# here would mean either retyping the whole config as a Nix attrset — losing
# the pairing between the `.secret_key` file and the `enc2:`-encrypted values
# in config.toml, which must travel together as opaque bytes since they're
# tied to that key — or leaking secrets into the world-readable /nix/store.
# Instead this mirrors the hand-rolled `systemd.services.meridian` pattern
# already used in agents.nix: infra (user, service, reverse proxy) is
# declarative; config/state is copied byte-for-byte from an existing
# ~/.zeroclaw (see the deploy notes below), owned by the service user.
{
  configurations.nixos.sisko.module =
    {
      config,
      pkgs,
      lib,
      ...
    }:
    {
      users.groups.zeroclaw = { };
      users.users.zeroclaw = {
        group = "zeroclaw";
        home = "/var/lib/zeroclaw";
        isSystemUser = true;
        createHome = true;
      };

      systemd.services.zeroclaw = {
        description = "ZeroClaw personal AI assistant daemon";
        after = [ "network-online.target" ];
        wants = [ "network-online.target" ];
        wantedBy = [ "multi-user.target" ];
        # The agent's shell tool execs "sh" (and other CLI tools like git,
        # docker) by bare name, not absolute path. The hardened unit's
        # default PATH is minimal, so point it at the same system profile an
        # interactive root shell already uses (environment.systemPackages).
        path = [ "/run/current-system/sw" ];
        serviceConfig = {
          Type = "simple";
          User = "zeroclaw";
          WorkingDirectory = config.users.users.zeroclaw.home;
          # --config-dir replaces the default ~/.zeroclaw with the state dir
          # populated out-of-band (config.toml + .secret_key + data/).
          ExecStart = "${lib.getExe pkgs.llm-agents.zeroclaw} daemon --config-dir ${config.users.users.zeroclaw.home}";
          Restart = "always";
          RestartSec = 5;
          ProtectSystem = "full";
          ProtectHome = "read-only";
          PrivateTmp = true;
          NoNewPrivileges = true;
        };
      };

      # Gateway binds 127.0.0.1:42617 by default (config.toml has no
      # [gateway] override) — matches the loopback + reverse-proxy pattern
      # already used for meridian.
      services.nginx.virtualHosts."zeroclaw.sisko.wg.aciceri.dev" = {
        forceSSL = true;
        useACMEHost = "aciceri.dev";
        locations."/" = {
          proxyPass = "http://127.0.0.1:42617";
          proxyWebsockets = true;
          extraConfig = ''
            proxy_buffering off;
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

      environment.persistence."/persist".directories = [
        "/var/lib/zeroclaw"
      ];
    };
}
