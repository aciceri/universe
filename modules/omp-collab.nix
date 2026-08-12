# Self-hosted omp collab stack: relay on sisko, sessions dashboard on pike.
# Clients:
#   omp config set collab.relayUrl wss://collab.sisko.wg.aciceri.dev
#   omp config set collab.webUrl https://my.omp.sh
{
  configurations.nixos.pike.module =
    { pkgs, lib, ... }:
    {
      # WireGuard peers only: the pages hand out collab room secrets.
      networking.firewall.interfaces."wg-universe".allowedTCPPorts = [ 80 ];

      systemd.services.omp-collab-dashboard = {
        description = "omp collab sessions dashboard";
        wantedBy = [ "multi-user.target" ];
        after = [ "network.target" ];
        serviceConfig = {
          ExecStart = lib.getExe pkgs.omp-collab-dashboard;
          # Runs as ccr to read (and prune) ~/.omp/run/collab-hosts.
          User = "ccr";
          Group = "users";
          Environment = [ "OMP_COLLAB_HOSTS_DIR=/home/ccr/.omp/run/collab-hosts" ];
          AmbientCapabilities = [ "CAP_NET_BIND_SERVICE" ];
          Restart = "on-failure";
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
        # Dashboard runs on pike, reached over WireGuard by IP.
        locations."/".proxyPass = "http://10.100.0.8:80";
        extraConfig = ''
          allow 10.100.0.0/24;
          allow 10.100.1.0/24;
          allow 127.0.0.1;
          deny all;
        '';
      };
    };
}
