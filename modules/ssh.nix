{ config, lib, ... }:
{
  flake.modules.nixos.ssh = {
    services.openssh = {
      enable = true;
      settings.PasswordAuthentication = false;
    };

    users.users.root.openssh.authorizedKeys.keys =
      lib.flip lib.filterAttrs config.users (_: user: user.god)
      |> lib.mapAttrsToList (_: user: user.sshKeys)
      |> lib.concatLists;
  };
}
