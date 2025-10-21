{
  configurations.nixos.sisko.module =
    { config, pkgs, ... }:
    let
      dir = "/mnt/hd/ncps";
      nixpkgs-ncps = builtins.getFlake "github:NixOS/nixpkgs/f5eabeba449f2c8ef60c2b273a759dd192df06ad";
    in
    {
      # FIXME remove once https://github.com/NixOS/nixpkgs/pull/445875 is merged
      disabledModules = [ "services/networking/ncps.nix" ];
      imports = [ "${nixpkgs-ncps}/nixos/modules/services/networking/ncps.nix" ];
      services.ncps.package = pkgs.ncps.overrideAttrs {
        vendorHash = "sha256-7bu9nXkS4Xfd2wEXIX+ANbVec1Lrh2w/zikOPeuAHzo=";
        src = pkgs.fetchFromGitHub {
          owner = "kalbasit";
          repo = "ncps";
          rev = "9de11fe9197404b66f0d51c2ee1f30fd9b6bbe59";
          hash = "sha256-krSbPECsdH91CtE5/MNNXRbPpTt1HZ5lqOGs3xG5WjM=";
        };
        doCheck = false;
      };

      secrets.nix_netrc = {
        mode = "770";
        group = "ncps";
      };

      systemd.tmpfiles.rules = [
        "d ${dir} 770 ncps ncps"
      ];

      services.ncps = {
        enable = true;
        cache = {
          hostName = "ncps.sisko.wg.aciceri.dev";
          dataPath = "${dir}/data";
          tempPath = "${dir}/tmp";
          databaseURL = "sqlite:${dir}/db/db.sqlite";
          maxSize = "200G";
          lru.schedule = "0 2 * * *"; # Clean up daily at 2 AM
          allowPutVerb = true;
          allowDeleteVerb = true;
        };
        server.addr = "0.0.0.0:8501";
        upstream = {
          caches = [
            "https://cache.nixos.org"
            "https://nix-community.cachix.org"
            "https://mlabs.cachix.org"
            "https://cache.iog.io"
            "http://sisko.wg.aciceri.dev:8081/sisko"
          ];
          publicKeys = [
            "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
            "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
            "mlabs.cachix.org-1:gStKdEqNKcrlSQw5iMW6wFCj3+b+1ASpBVY2SYuNV2M="
            "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
            "sisko:4A3G4hgZVjhfPLh7Hy9V6xhRzRJp1l4fDDbLqQrQsbU="
          ];
        };
        prometheus.enable = true;
        netrcFile = config.age.secrets.nix_netrc.path;
      };
    };
}
