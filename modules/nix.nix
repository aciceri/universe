{ inputs, ... }:
{
  flake.modules.nixos.base =
    { config, pkgs, ... }:
    {
      secrets.nix_netrc = { };

      nix = {
        package = pkgs.nixVersions.latest;

        optimise.automatic = true;

        settings = {
          auto-optimise-store = true;
          trusted-users = [
            "root"
            "@wheel"
          ];
          netrc-file = config.age.secrets.nix_netrc.path;
          substituters = [
            "http://ncps.sisko.wg.aciceri.dev:8501"
            "https://mlabs.cachix.org" # TODO move to ncps when https://github.com/kalbasit/ncps/discussions/223 is addressed
          ];
          trusted-public-keys = [
            "ncps.sisko.wg.aciceri.dev:lSep0cd5UkyKvDR9AtYargQslCAqMYI3Xin2w+LiZb0="
            "mlabs.cachix.org-1:gStKdEqNKcrlSQw5iMW6wFCj3+b+1ASpBVY2SYuNV2M="
          ];
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
          sshKey = "/etc/ssh/ssh_host_ed25519_key";
        }
      ];
    };
  };
}
