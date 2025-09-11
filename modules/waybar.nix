{ lib, ... }:
{
  flake.modules.homeManager.pc =
    { config, pkgs, ... }:
    {
      programs.waybar = {
        enable = true;
        systemd.enable = true;
        settings =
          let
            rofi = lib.getExe config.programs.rofi.package;
            pavucontrol = lib.getExe pkgs.pavucontrol;
            pactl = lib.getExe' pkgs.pulseaudio "pactl";
          in
          {
            mainBar = {
              layer = "top";
              position = "left";
              width = 36;
              margin = "12 0 12 0";
              spacing = 2;

              modules-left = [
                "clock"
                "custom/sep"
                "niri/window"
              ];

              modules-center = [
                "niri/workspaces"
              ];

              modules-right = [
                "tray"
                "custom/sep"
                "temperature"
                "custom/sep"
                "pulseaudio"
                "custom/powermenu"
              ];

              "custom/sep" = {
                format = "──";
              };

              "custom/powermenu" = {
                on-click = "${rofi} -show menu -modi menu:rofi-power-menu";
                format = "";
                tooltip = false;
              };

              "niri/workspaces" = {
                format = "{icon}";
                on-click = "activate";
                format-icons = {
                  active = "";
                  urgent = "";
                  default = "";
                };
              };

              "niri/window" = {
                rotate = 90;
                rewrite = {
                  "(.*) — Mozilla Firefox" = " $1";
                  "(.*) - Slack" = " $1";
                };
              };

              clock = {
                tooltip = true;
                format = "{:%H\n%M}";
                tooltip-format = "{:%Y-%m-%d}";
              };

              tray = {
                icon-size = 20;
                spacing = 5;
                show-passive-items = false;
              };

              temperature = {
                rotate = 90;
                hwmon-path = "/sys/class/hwmon/hwmon2/temp1_input";
                critical-threshold = 80;
                format = "{icon} {temperatureC}°C";
                format-icons = [
                  ""
                  ""
                  ""
                ];
              };

              pulseaudio = {
                rotate = 90;
                format = "{icon} {volume}%";
                format-bluetooth = "{icon} {volume}%";
                format-muted = "MUTE ";
                format-icons = {
                  headphones = "";
                  handsfree = "";
                  headset = "";
                  phone = "";
                  portable = "";
                  car = "";
                  default = [
                    ""
                    ""
                  ];
                };
                scroll-step = 3;
                on-click = pavucontrol;
                on-click-right = "${pactl} set-source-mute @DEFAULT_SOURCE@ toggle";
              };
            };
          };
        style = ''
          * {
            font-size: 1.35rem;
              font-weight: 600;
              font-family: "Iosevka Comfy", "Nerd Font";
            }

            window#waybar {
              border-top-right-radius: 5px;
              border-bottom-right-radius: 5px;
            }

            .modules-left,
            .modules-right,
            .modules-center {
              padding: 12px 4px;
            }

            tooltip,
            tooltip * {
              text-shadow: none;
            }

            #custom-sep {
              color: @base03;
            }

            #workspaces button {
              color: @base04;
              background: none;
              padding: 0;
            }

            #workspaces button:hover {
              color: @base0B;
              box-shadow: inherit;
              text-shadow: inherit;
            }

            #workspaces button.focused,
            #workspaces button.active {
              color: @base0E;
              border-bottom-color: @base0E;
            }

            #temperature {
              color: @base0A;
            }

            #clock {
              font-weight: 600;
              color: @base0C;
            }

            #custom-bluetooth_devices {
              color: @base0D;
            }

            #pulseaudio,
            #wireplumber,
            #sndio {
              color: @base0B;
            }

            #pulseaudio.muted,
            #wireplumber.muted,
            #sndio.muted {
              color: @base08;
            }

            #custom-powermenu {
              margin: 12px 0 0 0;
              color: @base04;
            }

            #upower,
            #battery {
              color: @base0A;
            }

            #upower.charging,
            #battery.charging {
              color: @base0B;
            }

            #network {
              color: @base0D;
            }

            #network.disconnected {
              color: @base08;
            }

            #backlight {
              color: @base09;
            }

            #cpu {
              color: @base0E;
            }

            #disk {
              color: @base0C;
            }

            #mpd {
              color: @base0E;
            }

            #memory {
              color: @base08;
            }

            #bluetooth {
              color: @base0D;
            }

            #bluetooth.disabled {
              color: @base03;
            }
        '';
      };

      home.packages = [
        pkgs.rofi-power-menu
      ];
    };
}
