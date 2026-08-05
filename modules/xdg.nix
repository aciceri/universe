{
  flake.modules.homeManager.base =
    { pkgs, ... }:
    {
      xdg.enable = true;
      # mimeApps is Linux-only (XDG MIME associations don't exist on darwin).
      xdg.mimeApps.enable = pkgs.stdenv.isLinux;
    };
}
