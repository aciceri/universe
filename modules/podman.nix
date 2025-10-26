{ config, lib, ... }:
{
  flake.modules.nixos.base =
    { pkgs, ... }:
    {
      boot.binfmt.preferStaticEmulators = true;

      virtualisation.podman = {
        enable = true;
        dockerCompat = true;
        defaultNetwork.settings.dns_enabled = true;
      };

      environment.systemPackages = with pkgs; [
        podman-compose
      ];

      # Winboat expects the user to be in the docker group
      users.groups.docker = { };
      users.users =
        config.users
        |> lib.mapAttrs (
          _: user: {
            extraGroups = lib.optional user.god "docker";
          }
        );
    };
}
