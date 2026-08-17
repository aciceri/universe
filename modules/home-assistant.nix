{ lib, ... }:
{
  configurations.nixos.sisko.module =
    { config, pkgs, ... }:
    {
      secrets = {
        home_assistant_ssh_key.owner = "hass";
        home_assistant_planimetry.owner = "hass";
      };

      services.home-assistant = {
        enable = true;
        package = pkgs.home-assistant.overrideAttrs (_: {
          # adding extra components causes a rebuild of this package
          # therefore checks are disabled to speed up the build
          doInstallCheck = false;
        });
        extraComponents = [
          "met"
          "radio_browser"
          "frontend"
          "cloud"
          "google_translate"
          "smartthings"
          "tuya"
          "timer"
          "cast"
          "weather"
          "backup"
          "brother"
          "webostv"
          "media_player"
          "wyoming"
          "wake_on_lan"
          "prometheus"
          "openai_conversation"
          "mcp_server"
          "shelly"
        ];
        customComponents = with pkgs.home-assistant-custom-components; [
          localtuya
        ];
        extraPackages =
          python3Packages: with python3Packages; [
            holidays
            beautifulsoup4
            getmac
            garminconnect
            tzlocal
            pyipp
          ];
        config = {
          default_config = { };
          http = {
            # nixpkgs dropped the `http.server_port` option default (and the
            # `openFirewall` option that used to read it), but the wg0 firewall
            # rule and the nginx proxy below still need it, so pin Home
            # Assistant's own default explicitly.
            server_port = 8123;
            use_x_forwarded_for = true;
            trusted_proxies = [
              "127.0.0.1"
              "::1"
            ];
          };
          homeassistant = {
            unit_system = "metric";
            time_zone = "Europe/Rome";
            temperature_unit = "C";
            external_url = "https://home.aciceri.dev";
          };
          logger.default = "WARNING";
          wake_on_lan = { };
          switch = [
            {
              name = "Picard";
              platform = "wake_on_lan";
              mac = "74:56:3c:37:17:bd"; # this shouldn't be public
              host = "picard.wg.aciceri.dev";
              turn_off.service = "shell_command.turn_off_picard";
            }
          ];
          shell_command.turn_off_picard = ''${lib.getExe pkgs.openssh} -i /var/lib/hass/.ssh/id_ed25519 -o StrictHostKeyChecking=no hass@picard.fleet "exec sudo \$(readlink \$(which systemctl)) poweroff"'';
          prometheus = {
            namespace = "hass";
          };
        };
      };

      systemd.tmpfiles.rules = [
        "d ${config.services.home-assistant.configDir}/custom_components 770 hass hass"
        "C+ ${config.services.home-assistant.configDir}/custom_components/pun_sensor 770 hass hass - ${pkgs.hass-pun-sensor}/custom_components/pun_sensor"
        "C+ ${config.services.home-assistant.configDir}/custom_components/garmin_connect 770 hass hass - ${pkgs.hass-garmin-connect}/custom_components/garmin_connect"

        "d ${config.services.home-assistant.configDir}/.ssh 770 hass hass"
        "C ${config.services.home-assistant.configDir}/.ssh/id_ed25519 700 hass hass - ${config.age.secrets.home_assistant_ssh_key.path}"

        "d ${config.services.home-assistant.configDir}/www 770 hass hass"
        "C ${config.services.home-assistant.configDir}/www/home.png 770 hass hass - - ${config.age.secrets.home_assistant_planimetry.path}"
      ];

      networking.firewall.interfaces."wg0" = {
        allowedTCPPorts = [
          config.services.home-assistant.config.http.server_port
          56000
        ];
      };

      virtualisation.oci-containers = {
        containers = {
          whisper = {
            image = "lscr.io/linuxserver/faster-whisper:latest";
            ports = [ "10300:10300" ];
            environment = {
              WHISPER_MODEL = "small-int8";
              WHISPER_LANG = "it";
              WHISPER_BEAM = "1";
            };
          };
          piper = {
            image = "rhasspy/wyoming-piper:latest";
            ports = [ "10200:10200" ];
            cmd = [
              "--voice"
              "it_IT-riccardo-x_low"
            ];
          };
        };
      };

      environment.persistence."/persist".directories = [
        config.services.home-assistant.configDir
      ];

      services.nginx.virtualHosts."home.aciceri.dev" = {
        forceSSL = true;
        enableACME = true;
        locations."/" = {
          proxyPass = "http://localhost:${toString config.services.home-assistant.config.http.server_port}";
          proxyWebsockets = true;
        };
        extraConfig = ''
          proxy_set_header    Upgrade     $http_upgrade;
          proxy_set_header    Connection  $connection_upgrade;
        '';
      };
    };
}
