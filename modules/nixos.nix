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
        options = {
          module = lib.mkOption {
            type = lib.types.deferredModule;
          };
          description = lib.mkOption {
            type = lib.types.str;
          };
        };
      }
    );
  };

  config = {
    flake = {
      nixosConfigurations =
        config.configurations.nixos
        |> lib.mapAttrs (name: { module, ... }: inputs.nixpkgs.lib.nixosSystem { modules = [ module ]; });

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

    readme.parts.nixos =
      let
        configurations =
          config.configurations.nixos
          |> lib.concatMapAttrsStringSep "\n" (
            name: nixos: ''
              ### `${name}`

              ${nixos.description}          
            ''
          );
      in
      ''
        ## NixOS configurations

        As you can notice I'm a big Star Trek fan...

        ${configurations}
      '';
  };
}
