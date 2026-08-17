{ inputs, lib, ... }:
let
  commonNix =
    { pkgs, ... }:
    {
      nix = {
        package = pkgs.nixVersions.latest;

        # Weekly hardlink dedup of the store. NOT `auto-optimise-store`: that
        # dedups inline on every store write, which on ZFS means a metadata
        # storm during every single build for the same end result.
        optimise.automatic = true;

        settings = {
          trusted-users = [
            "root"
            "@wheel"
          ];
          # Served by ncps on sisko (modules/ncps.nix, pinned to 0.10-rc which
          # fixes the 0.9 "invalid nar hash" 500s on opaque NAR URLs).
          substituters = [
            "http://ncps.sisko.wg.aciceri.dev:8501"
          ];
          trusted-public-keys = [
            "ncps.sisko.wg.aciceri.dev:jIWjtQrNKaYwklgkebFbqJIBeCkPl0ULSxSJ7YitxAQ="
          ];
        };
        nixPath = [ "nixpkgs=${inputs.nixpkgs}" ];
        extraOptions = ''
          experimental-features = nix-command flakes pipe-operators
        '';
        registry = rec {
          nixpkgs.to = {
            type = "path";
            path = inputs.nixpkgs;
          };
          n = nixpkgs;
        };
      };
    };
in
{
  flake.modules.nixos.base =
    { config, pkgs, ... }:
    {
      imports = [ (commonNix { inherit pkgs; }) ];

      secrets.nix_netrc = { };

      nix.settings.netrc-file = config.age.secrets.nix_netrc.path;
    };

  flake.modules.darwin.base =
    { config, pkgs, ... }:
    {
      imports = [ (commonNix { inherit pkgs; }) ];

      secrets.nix_netrc = { };

      nix.settings = {
        netrc-file = config.age.secrets.nix_netrc.path;
        trusted-users = [ "@admin" ];
      };
    };

  flake.modules.nixos.workstation = {
    boot.binfmt.emulatedSystems = [
      "i686-linux"
      "aarch64-linux"
      "riscv64-linux"
      "armv6l-linux"
    ];

    nix = {
      # Builds only get CPU/IO when the desktop is idle: full parallelism
      # when unattended, no jank while interactive.
      daemonCPUSchedPolicy = "idle";
      daemonIOSchedClass = "idle";
      extraOptions = ''
        extra-platforms = aarch64-linux arm-linux i686-linux riscv64-linux armv6l-linux
      '';
    };

    # nix-optimise.timer fires at 03:45 with Persistent=true, so on a laptop
    # that is asleep at night systemd runs it as boot catch-up instead -- i.e.
    # a full store sweep starting minutes after login. Nice=19 and
    # IOSchedulingClass=idle do not help: ZFS schedules through its own ZIO
    # pipeline and ignores CFQ/BFQ ionice, so the sweep saturates the pool and
    # every task blocks in txg_sync. Skip the missed run instead.
    # `Persistent = true` comes from the upstream module, hence mkForce.
    systemd.timers.nix-optimise.timerConfig.Persistent = lib.mkForce false;

    programs.nix-ld.enable = true;
    services.envfs.enable = true;
  };

  configurations.nixos.pike.module.nix.settings.max-jobs = 6;
  configurations.nixos.picard.module.nix.settings.max-jobs = 8;
  configurations.nixos.sisko.module.nix.settings.max-jobs = 2;
}
