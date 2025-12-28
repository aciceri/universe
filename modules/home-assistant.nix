{ lib, ... }:
{
  configurations.nixos.sisko.module =
    { config, pkgs, ... }:
    let
      pun_sensor = pkgs.fetchFromGitHub {
        owner = "virtualdj";
        repo = "pun_sensor";
        rev = "51b216fab5c0d454d66060647c36e81bebfaf059";
        hash = "sha256-bGVJx3bObXdf4AiC6bDvafs53NGS2aufRcTUmXy8nAI=";
      };
      garmin_connect = pkgs.fetchFromGitHub {
        owner = "cyberjunky";
        repo = "home-assistant-garmin_connect";
        rev = "e2deaed42b66c982b150ca9a9e543031ad51228c";
        hash = "sha256-TtrcgLGnhNRBF1SqKMkPlEi/XEBUtDAnaWfzkh50+D8=";
      };
    in
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
        openFirewall = true;
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
        "C+ ${config.services.home-assistant.configDir}/custom_components/pun_sensor 770 hass hass - ${pun_sensor}/custom_components/pun_sensor"
        "C+ ${config.services.home-assistant.configDir}/custom_components/garmin_connect 770 hass hass - ${garmin_connect}/custom_components/garmin_connect"

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
            image = "rhasspy/wyoming-whisper:latest";
            ports = [ "10300:10300" ];
            cmd = [
              "--model"
              "medium-int8"
              "--language"
              "it"
            ];
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
