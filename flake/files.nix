{ inputs, lib, ... }:
{
  imports = [ inputs.files.flakeModules.default ];

  perSystem =
    { config, ... }:

    let
      writer = config.files.writer.drv;
    in

    {
      make-shells.default = {
        shellHook = lib.getExe writer;
        packages = [ writer ];
      };

      pre-commit.settings.hooks.write-files = {
        enable = true;
        stages = [ "pre-commit" ];
        before = [ "treefmt" ];
        always_run = true;
        verbose = true;
        pass_filenames = false;
        entry = lib.getExe writer;
      };
    };
}
