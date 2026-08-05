# Builds a macOS-hosted QEMU VM for aarch64-linux NixOS configurations.
# Uses Homebrew's qemu-virgl for GPU acceleration (virtio-gpu-gl + ANGLE/Metal).
{
  config,
  inputs,
  lib,
  withSystem,
  ...
}:
let
  # Only build darwin VMs for aarch64-linux configurations.
  eligibleConfigs = lib.filterAttrs (
    _: nixos: nixos.config.nixpkgs.hostPlatform.system == "aarch64-linux"
  ) config.flake.nixosConfigurations;

  darwinVmModule =
    {
      config,
      lib,
      ...
    }:
    let
      consoles = lib.concatMapStringsSep " " (c: "console=${c}") config.virtualisation.qemu.consoles;
      regInfo = inputs.nixpkgs.legacyPackages.aarch64-darwin.closureInfo {
        rootPaths = config.virtualisation.additionalPaths;
      };
    in
    {
      virtualisation = {
        host.pkgs = inputs.nixpkgs.legacyPackages.aarch64-darwin;

        # QEMU wrapper that delegates to Homebrew's startergo/qemu at runtime.
        qemu.package = (
          inputs.nixpkgs.legacyPackages.aarch64-darwin.runCommand "qemu-virgl" { } ''
            mkdir -p $out/bin $out/share
            for bin in qemu-system-aarch64 qemu-img; do
              printf '#!/bin/sh\nexec /opt/homebrew/bin/%s "$@"\n' "$bin" > "$out/bin/$bin"
              chmod +x "$out/bin/$bin"
            done
            ln -s /opt/homebrew/opt/qemu/share/qemu $out/share/qemu
          ''
        );

        diskSize = 65536; # 64 GB
        memorySize = 8192; # 8 GB
        cores = 8;
        writableStoreUseTmpfs = false;

        # vmnet-shared instead of SLIRP (not compiled in startergo's QEMU).
        qemu.networkingOptions = lib.mkForce [
          "-device virtio-net-pci,netdev=net"
          "-netdev vmnet-shared,id=net"
        ];

        # mkForce to replace the default aarch64 virtio-gpu-pci with virtio-gpu-gl-pci.
        # We must re-include direct boot args since mkForce discards all defaults.
        qemu.options = lib.mkForce [
          # Direct boot (kernel/initrd passed directly, no bootloader needed)
          "-kernel \${NIXPKGS_QEMU_KERNEL_${config.system.name}:-${config.system.build.toplevel}/kernel}"
          "-initrd ${config.virtualisation.directBoot.initrd}"
          ''-append "$(cat ${config.system.build.toplevel}/kernel-params) init=${config.system.build.toplevel}/init regInfo=${regInfo}/registration ${consoles} $QEMU_KERNEL_PARAMS"''

          # Display + GPU (GL via ANGLE -> Metal on the host)
          # full-grab=on lets Ctrl-Alt-G capture *all* host keys for the guest,
          # including macOS system combos (Cmd-Tab, Cmd-Space, ...). Requires
          # granting QEMU "Input Monitoring" in System Settings > Privacy.
          # swap-opt-cmd makes Option/Command behave like Alt/Super in Linux.
          # full-screen starts maximized; zoom-to-fit scales the guest to fill
          # the screen (without it you get the native resolution + black bars).
          # Toggle full screen at runtime with Ctrl-Alt-F.
          "-display cocoa,gl=es,full-grab=on,swap-opt-cmd=on,full-screen=on,zoom-to-fit=on"
          "-device virtio-gpu-gl-pci"

          # Input
          "-device usb-ehci,id=usb0"
          "-device usb-kbd"
          "-device usb-tablet"

          # Audio (CoreAudio on the macOS host)
          "-audiodev coreaudio,id=audio0"
          "-device intel-hda"
          "-device hda-output,audiodev=audio0"

          # Clipboard sharing (SPICE vdagent)
          "-chardev qemu-vdagent,id=spice,name=vdagent,clipboard=on"
          "-device virtio-serial-pci"
          "-device virtserialport,chardev=spice,name=com.redhat.spice.0"
        ];
      };

      users.users.root.password = "nixos";
    };
in
{
  flake.packages =
    eligibleConfigs
    |> lib.mapAttrsToList (
      name: nixos:
      let
        nixosVM = nixos.extendModules {
          modules = [
            darwinVmModule
            (
              { modulesPath, ... }:
              {
                imports = [
                  "${modulesPath}/virtualisation/qemu-vm.nix"
                  "${modulesPath}/profiles/qemu-guest.nix"
                ];
              }
            )
          ];
        };
      in
      withSystem "aarch64-darwin" (_: {
        aarch64-darwin."vm/darwin/${name}" = nixosVM.config.system.build.vm // {
          meta = (nixosVM.config.system.build.vm.meta or { }) // {
            description = "NixOS VM '${name}' for macOS (GPU-accelerated via qemu-virgl)";
          };
        };
      })
    )
    |> lib.mkMerge;
}
