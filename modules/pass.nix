{ lib, ... }:
{
  flake.modules.nixos.workstation = {
    programs.ssh.knownHosts = {
      "git.sr.ht".publicKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIMZvRd4EtM7R+IHVMWmDkVU3VLQTSwQDSAvW0t2Tkj60";
    };
  };

  flake.modules.homeManager.workstation =
    { config, pkgs, ... }:
    let
      passSelector = pkgs.writers.writeNu "pass-selector.nu" { } ''
        def main [...args] {
          if ($args | is-empty) {
            glob "${config.programs.password-store.settings.PASSWORD_STORE_DIR}/**/*.gpg"
            | each { |path|
              $path
              | str replace "${config.programs.password-store.settings.PASSWORD_STORE_DIR}/" ""
              | str replace ".gpg" ""
            }
            | str join "\n"
          } else {
            let selection = ($args | str join " ")

            if ($selection | str contains -i "otp") or ($selection | str contains -i "totp") {
              ^${lib.getExe' config.programs.password-store.package "pass"} otp --clip $selection out+err> /dev/null
            } else {
              ^${lib.getExe' config.programs.password-store.package "pass"} show --clip $selection out+err> /dev/null
            }
          }
        }
      '';

      rofiPassSelector = pkgs.writeShellScriptBin "rofi-pass-selector" ''
        exec ${lib.getExe config.programs.rofi.package} -show pass -modi "pass:${passSelector}"
      '';
    in
    lib.mkIf (config.home.username == "ccr") {
      programs.password-store = {
        enable = true;
        package = pkgs.pass.withExtensions (e: [ e.pass-otp ]);
        settings = {
          PASSWORD_STORE_DIR = "${config.home.homeDirectory}/.password-store";
        };
      };

      services.git-sync = {
        enable = true;
        repositories.password-store = {
          path = config.programs.password-store.settings.PASSWORD_STORE_DIR;
          uri = "git@git.sr.ht:~zrsk/pass";
          interval = 1000;
        };
      };

      home.packages = [ rofiPassSelector ];
      programs.niri.settings.binds."Mod+p".action = config.lib.niri.actions.spawn (lib.getExe rofiPassSelector);
    };
}
