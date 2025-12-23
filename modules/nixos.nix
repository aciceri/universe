{
  config,
  inputs,
  lib,
  ...
}:
{
  options.configurations.nixos = lib.mkOption {
    type = lib.types.lazyAttrsOf (
      lib.types.submodule {
        options.module = lib.mkOption {
          type = lib.types.deferredModule;
        };
      }
    );
  };

  config = {
    flake = {
      nixosConfigurations =
        config.configurations.nixos
        |> lib.mapAttrs (name: { module, ... }: inputs.nixpkgs.lib.nixosSystem { modules = [ module ]; });

      homeConfigurations =
        config.flake.nixosConfigurations
        |> lib.concatMapAttrs (
          hostname: nixos:
          lib.mapAttrs' (username: hm: {
            name = "${username}@${hostname}";
            value = {
              config = hm;
            };
          }) nixos.config.home-manager.users
        );

      checks =
        config.flake.nixosConfigurations
        |> lib.mapAttrsToList (
          name: nixos: {
            ${nixos.config.nixpkgs.hostPlatform.system} = {
              "configurations/nixos/${name}" = nixos.config.system.build.toplevel;
            };
          }
        )
        |> lib.mkMerge;
    };

    readme.parts.nixos = lib.mkBefore ''
      ## NixOS configurations

      As you can notice I'm a big Star Trek fan...
    '';
  };
}
