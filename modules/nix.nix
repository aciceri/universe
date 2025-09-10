{ inputs, ... }:
{
  flake.modules.nixos.base =
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
}
