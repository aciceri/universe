{ config, lib, ... }:
{
  flake.modules.nixos.base = nArgs: {
    services.openssh = {
      enable = true;
      settings.PasswordAuthentication = false;
    };

    secrets = {
      "ssh_host_key_${nArgs.config.networking.hostName}" = { };
    }
    // (
      config.users
      |> lib.mapAttrs' (
        username: _:
        lib.nameValuePair "ssh_user_key_${username}" {
          owner = username;
        }
      )
    );

    users.users = {
      root.openssh.authorizedKeys.keys =
        lib.flip lib.filterAttrs config.users (_: user: user.god)
        |> lib.mapAttrsToList (_: user: user.sshKeys)
        |> lib.concatLists;
    }
    // (
      config.users
      |> lib.mapAttrs (
        username: _: {
          openssh.authorizedKeys.keys = [
            config.secrets."ssh_user_key_${username}".sshPublicKey # each user can logged by himself
            config.secrets."ssh_host_key_${nArgs.config.networking.hostName}".sshPublicKey # ..and by the host/root
          ];
        }
      )
    );
  };

  flake.modules.homeManager.base =
    {
      config,
      age,
      osConfig,
      ...
    }:
    {
      home.file.".ssh/id_ed25519".source =
        config.lib.file.mkOutOfStoreSymlink "${age.secretsDir}/ssh_user_key_${config.home.username}";
      programs.ssh = {
        enable = true;
        enableDefaultConfig = false;
        matchBlocks."*" = {
          setEnv.TERM = "xterm-256color";
          hashKnownHosts = false;
          identityFile =
            if config.home.username == "root" then
              age.secrets."ssh_host_key_${osConfig.networking.hostName}".path # the host shares the key with root
            else
              age.secrets."ssh_user_key_${config.home.username}".path;
        };
      };
    };
}
