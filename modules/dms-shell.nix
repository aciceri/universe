{
  flake.modules.nixos.pc = {
    programs.dms-shell = {
      enable = true;

      systemd = {
        enable = true;
        restartIfChanged = true;
      };

      enableSystemMonitoring = true;
      enableClipboard = true;
      enableVPN = true;
      enableDynamicTheming = true;
      enableAudioWavelength = true;
      enableCalendarEvents = false;
    };
  };

  flake.modules.homeManager.pc =
    {
      lib,
      config,
      ...
    }:
    {
      services.swaync.enable = lib.mkForce false;
      services.gammastep.enable = lib.mkForce false;
      services.swayidle.enable = lib.mkForce false;
      programs.swaylock.enable = lib.mkForce false;
      programs.rofi.enable = lib.mkForce false;
      programs.waybar.enable = lib.mkForce false;

      programs.niri.settings.layer-rules = lib.mkAfter [
        {
          matches = [ { namespace = "^quickshell$"; } ];
          place-within-backdrop = true;
        }
        {
          matches = [ { namespace = "^dms:blurwallpaper$"; } ];
          place-within-backdrop = true;
        }
      ];

      programs.niri.settings.window-rules = lib.mkAfter [
        {
          matches = [ { app-id = "^org\\.quickshell$"; } ];
          open-floating = true;
        }
      ];

      programs.niri.settings.binds = {
        "Mod+Space" = lib.mkForce {
          action = config.lib.niri.actions.spawn "dms" "ipc" "call" "spotlight" "toggle";
        };
        "Mod+D" = lib.mkForce {
          action = config.lib.niri.actions.spawn "dms" "ipc" "call" "spotlight" "toggle";
        };
        "Mod+W" = lib.mkForce {
          action = config.lib.niri.actions.spawn "dms" "ipc" "call" "spotlight" "toggle";
        };
        "Mod+V" = lib.mkForce {
          action = config.lib.niri.actions.spawn "dms" "ipc" "call" "clipboard" "toggle";
        };
        "Mod+O" = lib.mkForce {
          action = config.lib.niri.actions.spawn "dms" "ipc" "call" "processlist" "focusOrToggle";
        };
        "Mod+Shift+N" = lib.mkForce {
          action = config.lib.niri.actions.spawn "dms" "ipc" "call" "notifications" "toggle";
        };
        "Mod+Semicolon" = lib.mkForce {
          action = config.lib.niri.actions.spawn "dms" "ipc" "call" "settings" "focusOrToggle";
        };
        "Mod+Y" = lib.mkForce {
          action = config.lib.niri.actions.spawn "dms" "ipc" "call" "dankdash" "wallpaper";
        };
        "Mod+Alt+L" = lib.mkForce {
          action = config.lib.niri.actions.spawn "dms" "ipc" "call" "lock" "lock";
        };

        "XF86AudioRaiseVolume" = lib.mkForce {
          action = config.lib.niri.actions.spawn "dms" "ipc" "call" "audio" "increment" "5";
          allow-when-locked = true;
        };
        "XF86AudioLowerVolume" = lib.mkForce {
          action = config.lib.niri.actions.spawn "dms" "ipc" "call" "audio" "decrement" "5";
          allow-when-locked = true;
        };
        "XF86AudioMute" = lib.mkForce {
          action = config.lib.niri.actions.spawn "dms" "ipc" "call" "audio" "mute";
          allow-when-locked = true;
        };
        "XF86AudioMicMute" = lib.mkForce {
          action = config.lib.niri.actions.spawn "dms" "ipc" "call" "audio" "micmute";
          allow-when-locked = true;
        };

        "Mod+Shift+Period" = lib.mkForce {
          action = config.lib.niri.actions.spawn "dms" "ipc" "call" "brightness" "increment" "5" "";
        };
        "Mod+Shift+Comma" = lib.mkForce {
          action = config.lib.niri.actions.spawn "dms" "ipc" "call" "brightness" "decrement" "5" "";
        };
        "Mod+XF86MonBrightnessUp" = lib.mkForce {
          action = config.lib.niri.actions.spawn "dms" "ipc" "call" "brightness" "increment" "5" "";
        };
        "Mod+XF86MonBrightnessDown" = lib.mkForce {
          action = config.lib.niri.actions.spawn "dms" "ipc" "call" "brightness" "decrement" "5" "";
        };

        "Mod+S" = lib.mkForce {
          action = config.lib.niri.actions.spawn "dms" "ipc" "call" "niri" "screenshot";
        };
        "Mod+Ctrl+S" = lib.mkForce {
          action = config.lib.niri.actions.spawn "dms" "ipc" "call" "niri" "screenshotWindow";
        };
        "Mod+Alt+S" = lib.mkForce {
          action = config.lib.niri.actions.spawn "dms" "ipc" "call" "niri" "screenshotScreen";
        };

        "Mod+b" = lib.mkForce {
          action = lib.mkForce <| config.lib.niri.actions.spawn <| lib.getExe config.programs.zen-browser.package;
        };
      };

      programs.niri.settings.layout.background-color = lib.mkForce "transparent";
      home.file.".config/wallpaper.jpg".source = config.stylix.image;
    };
}
