{ inputs, lib, ... }:
{
  imports = [ inputs.files.flakeModules.default ];

  perSystem =
    { config, ... }:
    {
      make-shells.default =
        let
          writer = config.files.writer.drv;
        in
        {
          shellHook = lib.getExe writer;
          packages = [ writer ];
        };
    };
}
