{
  config,
  inputs,
  lib,
  rootPath,
  ...
}:
let

  secretSubmoduleType = lib.types.submodule (
    { name, ... }:
    {
      options = {
        name = lib.mkOption {
          type = lib.types.str;
          default = name;
        };
        file = lib.mkOption {
          type = lib.types.path;
          default = rootPath + /secrets + "/${name}" + ".age";
        };
        sshHostKey = lib.mkOption {
          type = lib.types.bool;
          default = lib.hasPrefix "ssh_host_key_" name;
        };
        sshPublicKey = lib.mkOption {
          type = lib.types.str; # TODO add assertion checking that if sshHostKey is true then this has to be set
          default = "";
        };
        publicKeys = lib.mkOption {
          type = lib.types.listOf lib.types.str;
          default = [ ];
        };
      };
    }
  );
in
{
  imports = [ inputs.agenix-shell.flakeModules.default ];

  options.secrets = lib.mkOption {
    type = lib.types.attrsOf secretSubmoduleType;
  };

  config = {
    agenix-shell.secrets = lib.mapAttrs' (name: value: {
      name = lib.toUpper name;
      value.file = value.file;
    }) config.secrets;

    perSystem =
      psArgs@{ pkgs, inputs', ... }:
      {
        make-shells.default = {
          shellHook = "source ${lib.getExe psArgs.config.agenix-shell.installationScript}";
          packages = [ inputs'.agenix.packages.agenix ];
          env.RULES =
            let
              gods = lib.flip lib.filterAttrs config.users (_: user: user.god);
              godsPublicKeys = gods |> lib.mapAttrsToList (_: user: user.sshKeys) |> lib.concatLists;
            in
            pkgs.writeText "secrets.nix" (
              lib.generators.toPretty { multiline = true; } (
                lib.flip lib.mapAttrs' config.secrets (
                  name: value: {
                    name = "${name}.age";
                    value.publicKeys = lib.unique (godsPublicKeys ++ value.publicKeys);
                  }
                )
              )
            );
        };
      };
  };

  config.secrets = {
    gptcommit__openai__api_key = { };
  };

  # SSH host keys
  config.secrets = {
    ssh_host_key_picard.sshPublicKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIJ1+1z1IsLVJ6aGarMgzw3NbmFKcpYVgdUjl7xDsewxT";
    ssh_host_key_sisko.sshPublicKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIKPBaKPx0HsJpGmMT//vo2GXvGh4ULoItq49ltCMzVw4";
  };

  # SSH user keys
  config.secrets = {
    ssh_user_key_ccr.sshPublicKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIIm9Sl/I+5G4g4f6iE4oCUJteP58v+wMIew9ZuLB+Gea";
  };
}
