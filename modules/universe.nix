fpArgs: {
  flake.modules.nixos.workstation = {
    programs.ssh.knownHosts = {
      "git.aciceri.dev".publicKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIKPBaKPx0HsJpGmMT//vo2GXvGh4ULoItq49ltCMzVw4";
    };
  };

  flake.modules.homeManager.base =
    { config, ... }:
    {
      services.git-fetch.repositories.universe = {
        path = "${config.home.homeDirectory}/universe";
        uri = "ssh://forgejo@git.aciceri.dev/aciceri/universe.git";
        interval = 1000;
      };
      nix.registry.universe.to = {
        type = "path";
        path = config.services.git-fetch.repositories.universe.path;
      };
    };
}
