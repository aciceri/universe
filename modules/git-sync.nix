fpArgs@{ lib, ... }:
{
  flake.modules.nixos.base = {
    programs.ssh.knownHosts = {
      "git.aciceri.dev".publicKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIKPBaKPx0HsJpGmMT//vo2GXvGh4ULoItq49ltCMzVw4";
    };
  };

  flake.modules.homeManager.base =
    { config, ... }:
    lib.mkIf fpArgs.config.users.${config.home.username}.god {
      services.git-sync = {
        enable = true;
        repositories.universe = {
          path = "${config.home.homeDirectory}/universe";
          uri = "ssh://forgejo@git.aciceri.dev/aciceri/universe.git";
          interval = 1000;
        };
      };
    };
}
