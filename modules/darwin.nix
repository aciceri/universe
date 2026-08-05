{
  config,
  inputs,
  lib,
  ...
}:
{
  options.configurations.darwin = lib.mkOption {
    type = lib.types.lazyAttrsOf (
      lib.types.submodule {
        options.module = lib.mkOption {
          type = lib.types.deferredModule;
        };
      }
    );
    default = { };
  };

  config = {
    flake = {
      darwinConfigurations =
        config.configurations.darwin
        |> lib.mapAttrs (name: { module, ... }: inputs.nix-darwin.lib.darwinSystem { modules = [ module ]; });

      homeConfigurations =
        config.flake.darwinConfigurations
        |> lib.concatMapAttrs (
          hostname: darwin:
          lib.mapAttrs' (username: hm: {
            name = "${username}@${hostname}";
            value = {
              config = hm;
            };
          }) darwin.config.home-manager.users
        );

      checks =
        config.flake.darwinConfigurations
        |> lib.mapAttrsToList (
          name: darwin: {
            ${darwin.config.nixpkgs.hostPlatform.system} = {
              "configurations/darwin/${name}" = darwin.config.system.build.toplevel;
            };
          }
        )
        |> lib.mkMerge;
    };

    readme.parts.darwin = lib.mkBefore ''
      ## nix-darwin configurations
    '';
  };
}
