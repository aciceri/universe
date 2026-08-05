# Kirk is a NixOS desktop VM (aarch64-linux) running on archer (aarch64-darwin).
# It imports the full `pc` module set and then mkForce-disables the parts that
# need x86_64-only packages or host hardware/secrets (sane, printing, wireguard, ...).
{
  config,
  lib,
  ...
}:
{
  configurations.nixos.kirk.module =
    { pkgs, ... }:
    {
      imports = with config.flake.modules.nixos; [ pc ];

      # Disable modules from base that need host-specific secrets or x86_64 hardware.
      hardware.sane.enable = lib.mkForce false;
      services.printing.enable = lib.mkForce false;
      networking.wireguard.enable = lib.mkForce false;
      services.zerotierone.enable = lib.mkForce false;
      virtualisation.docker.enable = lib.mkForce false;
      services.fail2ban.enable = lib.mkForce false;
      services.forgejo.enable = lib.mkForce false;
      services.alloy.enable = lib.mkForce false;
      services.prometheus.enable = lib.mkForce false;

      # Required by agenix for identity paths; also useful for SSH into the VM.
      services.openssh.enable = true;

      # Override agenix-based password with a plaintext one for the VM.
      # sshd is key-only (base ssh module), so this is console-only access;
      # root is reachable via sudo from ccr.
      users.users.ccr.hashedPasswordFile = lib.mkForce null;
      users.users.ccr.initialPassword = lib.mkForce "kirk";
      # Pipewire for audio in the VM.
      services.pipewire = {
        enable = true;
        alsa.enable = true;
        pulse.enable = true;
      };

      environment.systemPackages = with pkgs; [
        chromium
        foot # Wayland-native terminal (no GL, works well with virgl)
        mesa-demos # glxinfo, glmark2 for testing GPU acceleration
      ];

      home-manager.sharedModules = with config.flake.modules.homeManager; [
        base
        # Minimal niri HM config for the VM.
        (
          { pkgs, config, ... }:
          {
            programs.niri = {
              package = pkgs.niri;
              settings = {
                input.keyboard.xkb.layout = "us";
                input.warp-mouse-to-focus.enable = true;
                input.focus-follows-mouse.enable = true;
                hotkey-overlay.skip-at-startup = true;
                prefer-no-csd = true;

                layout = {
                  gaps = 16;
                  default-column-width.proportion = 0.5;
                  focus-ring = {
                    enable = true;
                    width = 2;
                  };
                };

                binds = with config.lib.niri.actions; {
                  "Mod+T".action = spawn "foot";
                  "Mod+Q".action = close-window;
                  "Mod+D".action = spawn "chromium";
                  "Mod+F".action = maximize-column;
                  "Mod+Shift+F".action = fullscreen-window;

                  "Mod+Left".action = focus-column-left;
                  "Mod+Right".action = focus-column-right;
                  "Mod+Up".action = focus-window-up;
                  "Mod+Down".action = focus-window-down;
                  "Mod+H".action = focus-column-left;
                  "Mod+L".action = focus-column-right;
                  "Mod+J".action = focus-window-down;
                  "Mod+K".action = focus-window-up;

                  "Mod+Ctrl+Left".action = move-column-left;
                  "Mod+Ctrl+Right".action = move-column-right;
                  "Mod+Ctrl+H".action = move-column-left;
                  "Mod+Ctrl+L".action = move-column-right;

                  "Mod+R".action = switch-preset-column-width;
                  "Mod+Shift+E".action = quit;
                };
              };
            };

            programs.foot.enable = true;
            programs.chromium.enable = true;
          }
        )
      ];
    };
}
