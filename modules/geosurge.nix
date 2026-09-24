{
  # geoSurge ships only as an x86_64 AppImage, wrapped in packages/geosurge.
  flake.modules.homeManager.workstation =
    { pkgs, lib, ... }:
    {
      home.packages = lib.optional (pkgs.stdenv.hostPlatform.system == "x86_64-linux") pkgs.geosurge;
    };
}
