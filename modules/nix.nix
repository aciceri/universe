{ inputs, ... }:
{
  flake.modules.nixos.base =
    { config, pkgs, ... }:
    {
      secrets.nix-netrc = { };

      nix = {
        package = pkgs.nixVersions.latest;

        optimise.automatic = true;

        settings = {
          auto-optimise-store = true;
          trusted-users = [
            "root"
            "@wheel"
          ];
          netrc-file = config.age.secrets.nix-netrc.path;
        };
        nixPath = [ "nixpkgs=${inputs.nixpkgs}" ];
        extraOptions = ''
          experimental-features = nix-command flakes pipe-operators
        '';
        gc = {
          automatic = true;
          dates = "weekly";
          options = "--delete-older-than 180d";
        };
        registry = rec {
          nixpkgs.to = {
            type = "path";
            path = inputs.nixpkgs;
          };
          n = nixpkgs;
        };
      };
    };

  flake.modules.nixos.workstation = {
    boot.binfmt.emulatedSystems = [
      "i686-linux"
      "aarch64-linux"
      "riscv64-linux"
      "armv6l-linux"
    ];
    nix = {
      extraOptions = ''
        extra-platforms = aarch64-linux arm-linux i686-linux riscv64-linux armv6l-linux
      '';
      distributedBuilds = true;
      buildMachines = [
        {
          hostName = "sisko.wg.aciceri.dev";
          system = "aarch64-linux";
          maxJobs = 7;
          supportedFeatures = [
            "kvm"
            "nixos-test"
            "big-parallel"
            "benchmark"
          ];
          protocol = "ssh-ng";
          sshUser = "root";
          sshKey = "/run/agenix/ssh_user_key_ccr";
          # sshKey = "/etc/ssh/ssh_host_ed25519_key";  # TODO use this once sisko is re-deployed
        }
      ];
    };
  };
}
