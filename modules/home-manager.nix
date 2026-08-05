{
  config,
  lib,
  inputs,
  ...
}:
let
  hmUsers =
    {
      sysArgs,
      hmStateVersion,
      includeRoot ? false,
    }:
    config.users // (lib.optionalAttrs includeRoot { root = { }; })
    |> lib.mapAttrs (
      username: _:
      (
        { pkgs, ... }:
        {
          imports = [
            {
              _module.args = { inherit (sysArgs.config) age; };
              home = {
                stateVersion = hmStateVersion;
                inherit username;
              };
              home.packages = [ pkgs.home-manager ];
              programs.home-manager.enable = true;
              systemd.user.startServices = "sd-switch";
            }
          ];
        }
      )
    );
in
{
  flake.modules.nixos.base = nixosArgs: {
    imports = [
      inputs.home-manager.nixosModules.home-manager
    ];

    secrets = config.users |> lib.mapAttrs' (username: _: lib.nameValuePair "nixos_password_${username}" { });
    users.mutableUsers = false;
    users.users =
      config.users
      |> lib.mapAttrs (
        username: user: {
          isNormalUser = true;
          hashedPasswordFile = nixosArgs.config.age.secrets."nixos_password_${username}".path;
        }
      );

    home-manager = {
      useGlobalPkgs = true;
      extraSpecialArgs.hasGlobalPkgs = true;
      users = hmUsers {
        sysArgs = nixosArgs;
        hmStateVersion = nixosArgs.config.system.stateVersion;
        includeRoot = true;
      };
    };
  };

  flake.modules.darwin.base = darwinArgs: {
    imports = [
      inputs.home-manager.darwinModules.home-manager
    ];

    home-manager = {
      useGlobalPkgs = true;
      extraSpecialArgs.hasGlobalPkgs = true;
      backupFileExtension = "hm-backup";
      users = hmUsers {
        sysArgs = darwinArgs;
        hmStateVersion = "26.05";
      };
    };
  };
}
