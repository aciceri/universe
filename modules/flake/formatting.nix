{ inputs, ... }:
{
  imports = with inputs; [
    treefmt-nix.flakeModule
    git-hooks.flakeModule
  ];

  perSystem =
    { config, ... }:
    {
      treefmt.programs = {
        nixfmt = {
          enable = true;
          width = 120;
        };
        nixf-diagnose.enable = true;
      };

      pre-commit.settings.hooks.treefmt.enable = true;

      make-shells.default.shellHook = config.pre-commit.installationScript;
    };

  gitignore = [ ".pre-commit-config.yaml" ];
}
