{
  config,
  lib,
  withSystem,
  ...
}:
let

  commonModule =
    { config, modulesPath, ... }:
    {
      facter.detected.dhcp.enable = false;
      imports = [
        "${modulesPath}/virtualisation/qemu-vm.nix"
        "${modulesPath}/profiles/qemu-guest.nix"
      ];
      services.openssh.hostKeys = [
        {
          path = "${config.virtualisation.sharedDirectories.ssh-keys.target}/ssh_host_ed25519_key";
          type = "ed25519";
        }
      ];
      users.users.root.password = "nixos";

      virtualisation = {
        forwardPorts = [
          {
            from = "host";
            host.port = 2222;
            guest.port = 22;
          }
        ];
        sharedDirectories = {
          ssh-keys = {
            source = "$PROVISIONED_SSH_KEYS";
            target = "/mnt/provisioned-host-ssh-keys";
          };
        };
      };
    };
in
{
  flake.packages =
    config.flake.nixosConfigurations
    |> lib.mapAttrsToList (
      name: nixos:
      let
        nixosVM = nixos.extendModules {
          modules = [ commonModule ];
        };
        inherit (nixosVM.config.nixpkgs.hostPlatform) system;
      in
      withSystem system (
        { pkgs, ... }:
        {
          ${system}."vm/nixos/${name}" = pkgs.writeShellScriptBin "vm-${name}.sh" ''
            export PROVISIONED_SSH_KEYS=$(mktemp -d -t ${name}-provisioned-ssh-keys-XXXXXXXX -p $XDG_RUNTIME_DIR)

            cleanup() {
              echo "Cleaning up $PROVISIONED_SSH_KEYS"
              if [ -n "$PROVISIONED_SSH_KEYS" ] && [ -d "$PROVISIONED_SSH_KEYS" ]; then
                shred -vfz -n 3 "$PROVISIONED_SSH_KEYS"/* 2>/dev/null || true
                rm -rf "$PROVISIONED_SSH_KEYS"
              fi
            }
            trap cleanup EXIT INT TERM

            echo "Temporary copying the SSH private key to $PROVISIONED_SSH_KEYS"
            cp $SSH_HOST_KEY_${lib.toUpper name}_PATH $PROVISIONED_SSH_KEYS/ssh_host_ed25519_key
            ${lib.getExe nixosVM.config.system.build.vm}
          '';
        }
      )
    )
    |> lib.mkMerge;

  gitignore = config.flake.nixosConfigurations |> lib.mapAttrsToList (name: _: "${name}.qcow2");
}
