{ config, lib, ... }:
let
  cfg = config.readme;
in
{
  options.readme = {
    parts = lib.mkOption {
      type = lib.types.lazyAttrsOf lib.types.lines;
    };
    order = lib.mkOption {
      type = lib.types.listOf lib.types.str;
    };
    _renderedText = lib.mkOption {
      type = lib.types.str;
      internal = true;
      default = cfg.order |> lib.map (e: cfg.parts.${e}) |> lib.concatLines;
    };
  };

  config = {
    perSystem =
      { pkgs, ... }:
      {
        files.files = [
          {
            path_ = "README.md";
            drv =
              pkgs.runCommand "README.md"
                {
                  nativeBuildInputs = [ pkgs.mdformat ];
                }
                ''
                  cat > $out << 'EOF'
                  ${cfg._renderedText}
                  EOF
                  mdformat $out
                '';
          }
        ];
      };

    readme = {
      order = [
        "intro"
        "files"
      ];
      parts.intro = ''
        # Universe

        This repository is my comprehensive monorepo containing personal projects
        and configurations that aren't intended for external contributions,
        though contributions are always welcome.

        It follows the [dendritic pattern](https://github.com/mightyiam/dendritic),
        meaning each file is a [flake-parts](https://flake.parts/) module.
        Also, this README is dynamically generated using a flake-parts module, with its
        source distributed throughout the repository.
      '';
    };
  };
}
