{ config, lib, ... }:
{
  flake.modules.nixos.base = {
    services.openssh = {
      enable = true;
      settings.PasswordAuthentication = false;
    };

    secrets =
      config.users
      |> lib.mapAttrs' (
        username: _:
        lib.nameValuePair "ssh_user_key_${username}" {
          owner = username;
        }
      );

    users.users.root.openssh.authorizedKeys.keys =
      lib.flip lib.filterAttrs config.users (_: user: user.god)
      |> lib.mapAttrsToList (_: user: user.sshKeys)
      |> lib.concatLists;
  };

  flake.modules.homeManager.base =
    { config, age, ... }:
    {
      programs.ssh = {
        enable = true;
        enableDefaultConfig = false;
        matchBlocks."*" = {
          setEnv.TERM = "xterm-256color";
          hashKnownHosts = false;
          identityFile = lib.optional (config.home.username != "root") age.secrets."ssh_user_key_${config.home.username}".path;
        };
      };
    };
}
