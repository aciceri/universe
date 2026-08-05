{ inputs, ... }:
let
  commonNix =
    { pkgs, ... }:
    {
      nix = {
        package = pkgs.nixVersions.latest;

        optimise.automatic = true;

        settings = {
          auto-optimise-store = true;
          trusted-users = [
            "root"
            "@wheel"
          ];
          # Served by ncps on sisko (modules/ncps.nix, pinned to 0.10-rc which
          # fixes the 0.9 "invalid nar hash" 500s on opaque NAR URLs).
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
in
{
  flake.modules.nixos.base =
    { config, pkgs, ... }:
    {
      imports = [ (commonNix { inherit pkgs; }) ];

      secrets.nix_netrc = { };

      nix.settings.netrc-file = config.age.secrets.nix_netrc.path;
    };

  flake.modules.darwin.base =
    { config, pkgs, ... }:
    {
      imports = [ (commonNix { inherit pkgs; }) ];

      secrets.nix_netrc = { };

      nix.settings = {
        netrc-file = config.age.secrets.nix_netrc.path;
        trusted-users = [ "@admin" ];
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
