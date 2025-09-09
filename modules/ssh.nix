{ config, lib, ... }:
{
  flake.modules.nixos.base = {
    services.openssh = {
      enable = true;
      settings.PasswordAuthentication = false;
    };

    users.users.root.openssh.authorizedKeys.keys =
      lib.flip lib.filterAttrs config.users (_: user: user.god)
      |> lib.mapAttrsToList (_: user: user.sshKeys)
      |> lib.concatLists;
  };

  flake.modules.homeManager.base = {
    programs.ssh = {
      enable = true;
      enableDefaultConfig = false;
      matchBlocks."*" = {
        setEnv.TERM = "xterm-256color";
        compression = true;
        identitiesOnly = true;
        hashKnownHosts = false;
      };
    };
  };
}
