{ config, lib, ... }:
{
  flake.modules.nixos.base =
    { pkgs, ... }:
    {
      boot.binfmt.preferStaticEmulators = true;

      virtualisation.docker = {
        enable = true;
        rootless = {
          enable = true;
          setSocketVariable = true;
        };
        autoPrune.enable = true;
      };

      environment.systemPackages = with pkgs; [
        podman-compose
      ];

      users.users =
        config.users
        |> lib.mapAttrs (
          _: user: {
            extraGroups = lib.optional user.god "docker";
          }
        );
    };
}
