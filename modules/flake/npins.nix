{
  config,
  lib,
  rootPath,
  ...
}:
{
  options.npins = {
    path = lib.mkOption {
      type = lib.types.path;
      default = rootPath + "/npins";
    };
    sources = lib.mkOption {
      type = lib.types.attrs;
      default = builtins.import config.npins.path;
    };
  };

  config = {
    perSystem =
      { pkgs, ... }:
      {
        make-shells.default.packages = [ pkgs.npins ];
        treefmt.settings.global.excludes = [
          "npins/default.nix"
          "npins/sources.json"
        ];
      };
  };
}
