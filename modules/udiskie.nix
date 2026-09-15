{
  # A tray automounter needs a graphical session, so this belongs to
  # `workstation` rather than `base`: on a headless host it would declare a
  # user unit that can never do anything useful.
  #
  # Linux-only: udiskie has no meaning on darwin, guard so the module stays
  # importable there.
  flake.modules.homeManager.workstation =
    { pkgs, lib, ... }:
    lib.mkIf pkgs.stdenv.isLinux {
      services.udiskie.enable = true;
    };

  flake.modules.nixos.base = {
    services.udisks2.enable = true;
  };
}
