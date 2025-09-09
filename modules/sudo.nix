{ config, lib, ... }:
{
  flake.modules.nixos.base = {
    security.sudo-rs.enable = true;
    users.users =
      config.users
      |> lib.mapAttrs (
        _: user: {
          extraGroups = lib.optional user.god "wheel";
        }
      );
  };
}
