{
  config,
  inputs,
  lib,
  ...
}:
{
  config.flake.modules.nixos.agenix = {
    imports = [ inputs.agenix.nixosModules.age ];
  };

  config.secrets =
    config.flake.nixosConfigurations
    |> lib.attrValues
    |> lib.map (
      nixos:
      nixos.config.age.secrets
      |> lib.mapAttrs (
        _: secret: {
          publicKeys = [ config.secrets."ssh_host_key_${nixos.config.networking.hostName}".sshPublicKey ];
        }
      )
    )
    |> lib.mkMerge;
}
