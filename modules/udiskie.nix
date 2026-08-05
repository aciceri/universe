{
  # Linux-only: udiskie has no meaning on darwin, guard so homeManager.base stays importable there
  flake.modules.homeManager.base =
    { pkgs, lib, ... }:
    lib.mkIf pkgs.stdenv.isLinux {
      services.udiskie.enable = true;
    };

  flake.modules.nixos.base = {
    services.udisks2.enable = true;
  };
}
