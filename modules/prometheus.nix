{ lib, ... }:
{
  configurations.nixos.sisko.module =
    { config, ... }:
    {
      secrets.home_assistant_api_token.owner = config.systemd.services.prometheus.serviceConfig.User;

      services.prometheus = {
        enable = true;
        checkConfig = false; # Otherwise it fails because it cannot access bearer_token_file at build time
        webExternalUrl = "https://status.wg.aciceri.dev";
        globalConfig.scrape_interval = "10s";
        scrapeConfigs = [
          {
            job_name = "hass";
            metrics_path = "/api/prometheus";
            bearer_token_file = config.age.secrets.home_assistant_api_token.path;
            static_configs = [
              {
                targets = [
                  "sisko.wg.aciceri.dev:${builtins.toString config.services.home-assistant.config.http.server_port}"
                ];
              }
            ];
          }
          {
            job_name = "node";
            static_configs = [
              {
                targets = lib.map (host: "${host}.wg.aciceri.dev:9100") [
                  "sisko"
                  "picard"
                  "kirk"
                  "pike"
                ];
              }
            ];
          }
          {
            job_name = "wireguard";
            static_configs = [
              {
                targets = lib.map (host: "${host}.wg.aciceri.dev:9586") [
                  "picard"
                  "kirk"
                  "pike"
                ];
              }
            ];
          }
          {
            job_name = "zfs";
            static_configs = [
              {
                targets = lib.map (host: "${host}.wg.aciceri.dev:9134") [
                  "picard"
                  "kirk"
                  "pike"
                ];
              }
            ];
          }
          {
            job_name = "restic";
            static_configs = [
              {
                targets = lib.map (host: "${host}.wg.aciceri.dev:9753") [ "sisko" ];
              }
            ];
          }
          {
            job_name = "postgres";
            static_configs = [
              {
                targets = lib.map (host: "${host}.wg.aciceri.dev:9187") [ "sisko" ];
              }
            ];
          }
          {
            job_name = "nginx";
            static_configs = [
              {
                targets = lib.map (host: "${host}.wg.aciceri.dev:9117") [ "sisko" ];
              }
            ];
          }
          {
            job_name = "ncps";
            static_configs = [
              {
                targets = lib.map (host: "${host}.wg.aciceri.dev:8501") [ "sisko" ];
              }
            ];
          }
          {
            job_name = "smartctl";
            static_configs = [
              {
                targets = lib.map (host: "${host}.wg.aciceri.dev:9633") [
                  "sisko"
                  "kirk"
                  "picard"
                  "pike"
                ];
              }
            ];
          }
        ];
      };

      environment.persistence."/persist".directories = [
        "/var/lib/${config.services.prometheus.stateDir}"
      ];
    };

  flake.modules.nixos.base =
    {
      config,
      lib,
      pkgs,
      ...
    }:
    let
      hostname = config.networking.hostName;
      mkFor = hosts: lib.mkIf (lib.elem hostname hosts);
    in
    {
      secrets.sisko_restic_password = { };

      services.prometheus.exporters = {
        node =
          mkFor
            [
              "sisko"
              "picard"
              "kirk"
              "pike"
            ]
            {
              enable = true;
              enabledCollectors = [
                "cpu"
                "conntrack"
                "diskstats"
                "entropy"
                "filefd"
                "filesystem"
                "loadavg"
                "mdadm"
                "meminfo"
                "netdev"
                "netstat"
                "stat"
                "time"
                "vmstat"
                "systemd"
                "logind"
                "interrupts"
                "ksmd"
                "textfile"
                "pressure"
              ];
              extraFlags = [
                "--collector.ethtool"
                "--collector.softirqs"
                "--collector.tcpstat"
                "--collector.wifi"
              ];
            };
        wireguard =
          mkFor
            [
              "sisko"
              "picard"
              "kirk"
              "pike"
            ]
            {
              enable = true;
            };
        zfs =
          mkFor
            [
              "picard"
              "kirk"
              "pike"
            ]
            {
              enable = true;
            };
        restic = mkFor [ "sisko" ] {
          # https://github.com/ngosang/restic-exporter/issues/31
          enable = false;
          repository = config.services.restic.backups.sisko.repository;
          passwordFile = config.age.secrets.sisko_restic_password.path;
        };
        postgres = mkFor [ "sisko" ] {
          enable = true;
        };
        nginx = mkFor [ "sisko" ] {
          enable = true;
        };
        smartctl =
          mkFor
            [
              "sisko"
              "picard"
              "kirk"
              "pike"
            ]
            {
              enable = true;
            };
      };

      systemd.services.prometheus-restic-exporter.path = [ pkgs.openssh ];
    };
}
