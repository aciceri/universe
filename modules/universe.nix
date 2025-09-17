fpArgs: {
  flake.modules.nixos.workstation = {
    programs.ssh.knownHosts = {
      "git.aciceri.dev".publicKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIKPBaKPx0HsJpGmMT//vo2GXvGh4ULoItq49ltCMzVw4";
    };
  };

  flake.modules.homeManager.base =
    { config, ... }:
    let
      universePath = "${config.home.homeDirectory}/universe";
    in
    {
      services.git-fetch.repositories.universe = {
        path = universePath;
        uri = "ssh://forgejo@git.aciceri.dev/aciceri/universe.git";
        interval = 1000;
      };

      nix.registry = rec {
        universe.to = {
          type = "path";
          path = universePath;
        };
        u = universe;
      };

      programs.nushell.extraConfig = ''
        def hm-activate [] {
          let host = (hostname | str trim)
          let user = (whoami | str trim)
          nix run $"${universePath}#nixosConfigurations.\"($host)\".config.home-manager.users.\"($user)\".home.activationPackage"
        }

        def nixos [action: string] {
          let host = (hostname | str trim)
          nixos-rebuild $action --flake $"${universePath}#($host)" --sudo --print-build-logs --no-reexec --fast
        }

        def nixos-deploy [action: string, host: string] {
          nixos-rebuild $action --flake $"${universePath}#($host)" --target-host $"root@($host).wg.aciceri.dev" --sudo --print-build-logs --no-reexec --fast
         }
      '';
    };
}
