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
          ];
          trusted-public-keys = [
            "ncps.sisko.wg.aciceri.dev:jIWjtQrNKaYwklgkebFbqJIBeCkPl0ULSxSJ7YitxAQ="
          ];
        };
        nixPath = [ "nixpkgs=${inputs.nixpkgs}" ];
        extraOptions = ''
          experimental-features = nix-command flakes pipe-operators
        '';
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
    };

    programs.nix-ld.enable = true;
    services.envfs.enable = true;
  };
}
