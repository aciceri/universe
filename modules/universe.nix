{ lib, ... }:
{
  flake.modules.nixos.workstation = {
    programs.ssh.knownHosts = {
      "git.aciceri.dev".publicKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIKPBaKPx0HsJpGmMT//vo2GXvGh4ULoItq49ltCMzVw4";
    };
  };

  flake.modules.homeManager.base =
    { config, ... }:
    {
      options.universePath = lib.mkOption {
        type = lib.types.path;
        default = "${config.home.homeDirectory}/universe";
      };
      config = {
        services.git-fetch.repositories.universe = {
          path = config.universePath;
          uri = "ssh://forgejo@git.aciceri.dev/aciceri/universe.git";
          interval = 1000;
        };

        nix.registry = rec {
          universe.to = {
            type = "path";
            path = config.universePath;
          };
          u = universe;
        };
      };
    };
}
