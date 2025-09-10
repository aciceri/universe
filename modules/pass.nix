{ lib, ... }:
{
  flake.modules.nixos.workstation = {
    programs.ssh.knownHosts = {
      "git.sr.ht".publicKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIMZvRd4EtM7R+IHVMWmDkVU3VLQTSwQDSAvW0t2Tkj60";
    };
  };

  flake.modules.homeManager.workstation =
    { config, pkgs, ... }:
    lib.mkIf (config.home.username == "ccr") {
      programs.password-store = {
        enable = true;
        package = pkgs.pass.withExtensions (e: [ e.pass-otp ]);
        settings = {
          PASSWORD_STORE_DIR = "${config.home.homeDirectory}/.password-store";
        };
      };

      services.git-sync.repositories.password-store = {
        path = config.programs.password-store.settings.PASSWORD_STORE_DIR;
        uri = "git@git.sr.ht:~zrsk/pass";
        interval = 1000;
      };
    };
}
