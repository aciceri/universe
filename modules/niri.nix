{ inputs, lib, ... }:
{
  flake.modules.nixos.pc =
    { pkgs, ... }:
    {
      imports = [ inputs.niri.nixosModules.niri ];
      programs.niri = {
        enable = true;
        package = pkgs.niri;
      };
      niri-flake.cache.enable = false;
      graphicalSessions.niri.exec = lib.getExe' pkgs.niri "niri-session";
      security.pam.services.swaylock = { };
    };

  flake.modules.homeManager.pc =
    {
      pkgs,
      config,
      osConfig,
      ...
    }:
    {
      programs.niri = {
        package = pkgs.niri;
        settings = {
          input = {
            keyboard.xkb.layout = "us";

            touchpad = {
              tap = true;
              dwt = true;
              natural-scroll = true;
            };

            warp-mouse-to-focus.enable = true;

            focus-follows-mouse = {
              enable = true;
              max-scroll-amount = "0%";
            };
          };

          xwayland-satellite = {
            enable = true;
            path = lib.getExe pkgs.xwayland-satellite;
          };

          hotkey-overlay.skip-at-startup = true;
          prefer-no-csd = true;
          screenshot-path = "~/Pictures/Screenshots/Screenshot from %Y-%m-%d %H-%M-%S.png";

          spawn-at-startup = [
            {
              argv = [
                (lib.getExe pkgs.swaybg)
                "--image"
                config.stylix.image.outPath
                "--mode"
                "fill"
              ];
            }
          ];

          outputs = {
            "eDP-1" = {
              mode = {
                width = 1920;
                height = 1080;
                refresh = 59.999;
              };
              scale = 1.1;
              transform.rotation = 0;
            };

            "Dell Inc. DELL U2515H 9X2VY5170PJL" = {
              mode = {
                width = 2560;
                height = 1440;
                refresh = 59.951;
              };
              scale = 1.0;
              transform.rotation = 0;
            };
          };

          layout = {
            gaps = 16;
            center-focused-column = "never";

            preset-column-widths = [
              { proportion = 0.33333; }
              { proportion = 0.5; }
              { proportion = 0.66667; }
            ];

            default-column-width.proportion = 0.5;

            focus-ring = {
              enable = true;
              width = 2;
            };

            border = {
              enable = false;
              width = 4;
            };

            shadow.enable = true;
          };

          window-rules = [
            {
              matches = [ { } ];
              geometry-corner-radius = {
                top-left = 4.0;
                top-right = 4.0;
                bottom-left = 4.0;
                bottom-right = 4.0;
              };
              clip-to-geometry = true;
            }

            # Slack
            {
              matches = [ { app-id = "^Slack$"; } ];
              open-maximized = true;
            }

            # bTop
            {
              matches = [ { title = "^bTop$"; } ];
              opacity = 0.95;
              open-floating = true;
              open-focused = true;
            }

            # Claude desktop
            {
              matches = [ { title = "^Claude$"; } ];
              open-floating = true;
              open-focused = true;
            }

            # Authentication dialogs
            {
              matches = [ { title = "^Authentication Required"; } ];
              open-floating = true;
              open-focused = true;
            }
          ];

          layer-rules = [
            # Rofi shadow
            {
              matches = [
                { namespace = "^rofi$"; }
                { namespace = "^Claude$"; }
                { namespace = "^bTop$"; }
              ];
              shadow.enable = true;
            }
          ];

          binds =
            with config.lib.niri.actions;
            let
              floatingSize =
                {
                  picard = {
                    btop = {
                      rows = "60";
                      cols = "210";
                    };
                  };
                  kirk = {
                    btop = {
                      rows = "40";
                      cols = "140";
                    };
                  };
                }
                ."${osConfig.networking.hostName}" or {
                  btop = {
                    rows = "40";
                    cols = "140";
                  };
                };
              rofi = lib.getExe config.programs.rofi.package;
              rofi-pass = lib.getExe config.programs.rofi.pass.package;
              firefox = lib.getExe config.programs.firefox.package;
              footclient = lib.getExe' config.programs.foot.package "footclient";
              wpctl = lib.getExe' pkgs.wireplumber "wpctl";
              brightnessctl = lib.getExe pkgs.brightnessctl;
              spotube = lib.getExe pkgs.spotube;
              thunderbird = lib.getExe config.programs.thunderbird.package;
              run-floating-btop =
                with floatingSize.btop;
                pkgs.writeScriptBin "run-floating-btop" ''
                  foot --title='bTop' -W ${cols}x${rows} btop
                ''
                |> lib.getExe;
            in
            {
              # Help
              "Mod+Shift+Slash".action = show-hotkey-overlay;

              # Application launchers
              "Mod+T".action.spawn = footclient;
              "Mod+D".action = spawn rofi "-show" "drun";
              "Mod+W".action = spawn rofi "-show" "window";
              "Mod+E".action = toggle-column-tabbed-display;
              "Mod+P".action = spawn rofi-pass "--clip";
              "Mod+B".action = spawn firefox;
              "Mod+G".action = spawn "claude-desktop";
              "Mod+M".action = spawn spotube;
              "Mod+Shift+M".action = spawn thunderbird;
              # "Mod+Alt+L".action = spawn "swaylock";
              "Mod+Space".action = spawn rofi "-show" "menu" "-modi" "menu:rofi-power-menu";
              "Mod+Ctrl+B".action = spawn run-floating-btop;

              # Audio controls
              "XF86AudioRaiseVolume" = {
                action = spawn wpctl "set-volume" "@DEFAULT_AUDIO_SINK@" "0.1+";
                allow-when-locked = true;
              };
              "XF86AudioLowerVolume" = {
                action = spawn wpctl "set-volume" "@DEFAULT_AUDIO_SINK@" "0.1-";
                allow-when-locked = true;
              };
              "XF86AudioMute" = {
                action = spawn wpctl "set-mute" "@DEFAULT_AUDIO_SINK@" "toggle";
                allow-when-locked = true;
              };
              "XF86AudioMicMute" = {
                action = spawn wpctl "set-mute" "@DEFAULT_AUDIO_SOURCE@" "toggle";
                allow-when-locked = true;
              };

              # Brightness controls
              "Mod+Shift+Period".action = spawn brightnessctl "s" "5%+";
              "Mod+Shift+Comma".action = spawn brightnessctl "s" "5%-";
              "Mod+XF86MonBrightnessUp".action = spawn brightnessctl "s" "5%+";
              "Mod+XF86MonBrightnessDown".action = spawn brightnessctl "s" "5%-";

              # Window management
              "Mod+Q".action = close-window;

              # Focus movement
              "Mod+Left".action = focus-column-left;
              "Mod+Down".action = focus-window-down;
              "Mod+Up".action = focus-window-up;
              "Mod+Right".action = focus-column-right;
              "Mod+H".action = focus-column-left;
              "Mod+J".action = focus-window-down;
              "Mod+K".action = focus-window-up;
              "Mod+L".action = focus-column-right;

              # Window/column movement
              "Mod+Ctrl+Left".action = move-column-left;
              "Mod+Ctrl+Down".action = move-window-down;
              "Mod+Ctrl+Up".action = move-window-up;
              "Mod+Ctrl+Right".action = move-column-right;
              "Mod+Ctrl+H".action = move-column-left;
              "Mod+Ctrl+J".action = move-window-down;
              "Mod+Ctrl+K".action = move-window-up;
              "Mod+Ctrl+L".action = move-column-right;

              # Column positioning
              "Mod+Home".action = focus-column-first;
              "Mod+End".action = focus-column-last;
              "Mod+Ctrl+Home".action = move-column-to-first;
              "Mod+Ctrl+End".action = move-column-to-last;

              # Monitor focus
              "Mod+Shift+Left".action = focus-monitor-left;
              "Mod+Shift+Down".action = focus-monitor-down;
              "Mod+Shift+Up".action = focus-monitor-up;
              "Mod+Shift+Right".action = focus-monitor-right;
              "Mod+Shift+H".action = focus-monitor-left;
              "Mod+Shift+J".action = focus-monitor-down;
              "Mod+Shift+K".action = focus-monitor-up;
              "Mod+Shift+L".action = focus-monitor-right;

              # Move columns between monitors
              "Mod+Shift+Ctrl+Left".action = move-column-to-monitor-left;
              "Mod+Shift+Ctrl+Down".action = move-column-to-monitor-down;
              "Mod+Shift+Ctrl+Up".action = move-column-to-monitor-up;
              "Mod+Shift+Ctrl+Right".action = move-column-to-monitor-right;
              "Mod+Shift+Ctrl+H".action = move-column-to-monitor-left;
              "Mod+Shift+Ctrl+J".action = move-column-to-monitor-down;
              "Mod+Shift+Ctrl+K".action = move-column-to-monitor-up;
              "Mod+Shift+Ctrl+L".action = move-column-to-monitor-right;

              # Workspace navigation
              "Mod+Page_Down".action = focus-workspace-down;
              "Mod+Page_Up".action = focus-workspace-up;
              "Mod+U".action = focus-workspace-down;
              "Mod+I".action = focus-workspace-up;
              "Mod+Ctrl+Page_Down".action = move-column-to-workspace-down;
              "Mod+Ctrl+Page_Up".action = move-column-to-workspace-up;
              "Mod+Ctrl+U".action = move-column-to-workspace-down;
              "Mod+Ctrl+I".action = move-column-to-workspace-up;

              # Workspace movement
              "Mod+Shift+Page_Down".action = move-workspace-down;
              "Mod+Shift+Page_Up".action = move-workspace-up;
              "Mod+Shift+U".action = move-workspace-down;
              "Mod+Shift+I".action = move-workspace-up;

              # Mouse wheel bindings
              "Mod+WheelScrollDown" = {
                action = focus-workspace-down;
                cooldown-ms = 150;
              };
              "Mod+WheelScrollUp" = {
                action = focus-workspace-up;
                cooldown-ms = 150;
              };
              "Mod+Ctrl+WheelScrollDown" = {
                action = move-column-to-workspace-down;
                cooldown-ms = 150;
              };
              "Mod+Ctrl+WheelScrollUp" = {
                action = move-column-to-workspace-up;
                cooldown-ms = 150;
              };

              "Mod+WheelScrollRight".action = focus-column-right;
              "Mod+WheelScrollLeft".action = focus-column-left;
              "Mod+Ctrl+WheelScrollRight".action = move-column-right;
              "Mod+Ctrl+WheelScrollLeft".action = move-column-left;

              "Mod+Shift+WheelScrollDown".action = focus-column-right;
              "Mod+Shift+WheelScrollUp".action = focus-column-left;
              "Mod+Ctrl+Shift+WheelScrollDown".action = move-column-right;
              "Mod+Ctrl+Shift+WheelScrollUp".action = move-column-left;

              # Numbered workspaces
              "Mod+1".action = focus-workspace 1;
              "Mod+2".action = focus-workspace 2;
              "Mod+3".action = focus-workspace 3;
              "Mod+4".action = focus-workspace 4;
              "Mod+5".action = focus-workspace 5;
              "Mod+6".action = focus-workspace 6;
              "Mod+7".action = focus-workspace 7;
              "Mod+8".action = focus-workspace 8;
              "Mod+9".action = focus-workspace 9;

              # Window manipulation
              "Mod+Comma".action = consume-window-into-column;
              "Mod+Period".action = expel-window-from-column;
              "Mod+BracketLeft".action = consume-or-expel-window-left;
              "Mod+BracketRight".action = consume-or-expel-window-right;

              # Sizing
              "Mod+R".action = switch-preset-column-width;
              "Mod+Shift+R".action = switch-preset-window-height;
              "Mod+Ctrl+R".action = reset-window-height;
              "Mod+F".action = maximize-column;
              "Mod+Shift+F".action = fullscreen-window;
              "Mod+C".action = center-column;

              # Fine width adjustments
              "Mod+Minus".action = set-column-width "-10%";
              "Mod+Equal".action = set-column-width "+10%";

              # Fine height adjustments
              "Mod+Shift+Minus".action = set-window-height "-10%";
              "Mod+Shift+Equal".action = set-window-height "+10%";

              # Screenshots
              "Mod+S".action = screenshot;
              "Mod+Ctrl+S".action = screenshot-window;

              # System
              "Mod+Shift+E".action = quit;
              "Mod+Ctrl+Shift+E".action = quit { skip-confirmation = true; };
              "Ctrl+Alt+Delete".action = quit;
              "Mod+Shift+P".action = power-off-monitors;
            };
        };
      };

      programs.swaylock = {
        enable = true;
        package = pkgs.swaylock-effects;
        settings = {
          clock = true;
          screenshots = true;
          indicator = true;
          indicator-thickness = "20";
          indicator-radius = "100";
          effect-blur = "7x5";
          effect-vignette = "0.5:0.5";
          fade-in = "0.2";
        };
      };

      programs.zathura.enable = true;

      programs.imv.enable = true;

      services = {
        swaync = {
          enable = true;
          style = lib.mkAfter ''
            .notification-content {
              border: none;  /* Remove the external border */
            }
          '';
        };

        gammastep = {
          enable = true;
          provider = "geoclue2";
          tray = true;
        };

        swayidle =
          let
            swaylockCommand = lib.getExe config.programs.swaylock.package;
          in
          {
            enable = true;
            events = [
              {
                event = "before-sleep";
                command = swaylockCommand;
              }
              {
                event = "lock";
                command = swaylockCommand;
              }
            ];
            timeouts = [
              {
                timeout = 600; # 10 minutes
                command = swaylockCommand;
              }
              {
                timeout = 720; # 12 minutes
                command = "${lib.getExe' pkgs.systemd "systemctl"} suspend";
              }
            ];
          };
      };

      xdg.portal = {
        xdgOpenUsePortal = true;
        config.common.default = "gnome";
        extraPortals = [ pkgs.xdg-xdg-desktop-portal-gtk ];
      };

      home.packages = with pkgs; [
        nautilus # needed by the portal for the files picker
        wl-clipboard-rs
      ];
    };
}
