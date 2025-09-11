{
  config,
  lib,
  inputs,
  ...
}:
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
      users =
        config.users // { root = { }; }
        |> lib.mapAttrs (
          username: _:
          (
            { pkgs, ... }:
            {
              imports = [
                {
                  _module.args = { inherit (nixosArgs.config) age; };
                  home = {
                    stateVersion = nixosArgs.config.system.stateVersion;
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
    };
  };
}
