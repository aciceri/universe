# TODO re-generate the workflows as a pre-commit-hook
{
  config,
  lib,
  ...
}:
let

  buildDerivationJob = name: drvFlakePath: {
    runs-on = "native";
    steps = [
      {
        name = "Checkout repository";
        uses = "actions/checkout@v4";
      }
      {
        name = "Attic login";
        run = "attic login sisko http://sisko.wg.aciceri.dev:8081 $\{{secrets.ATTIC_SISKO_TOKEN}} --set-default";
      }
      {
        name = "Build";
        run = ''nix build ".#${drvFlakePath}" -L'';
      }
      {
        name = "Push to Attic";
        run = "attic push sisko result";
      }
    ];
  };

  toRemove = [
    "files/.gitignore"
    "files/.forgejo/workflows/build-checks.yaml"
    "files/.forgejo/workflows/update-flake-lock.yaml"
    "files/.forgejo/workflows/deploy.yaml"
  ]; # FIXME `nix build` doesn't like dots in the derivation path

  buildJobs =
    (
      (lib.removeAttrs config.flake.checks.x86_64-linux toRemove)
      |> lib.mapAttrs' (
        name: _:
        lib.nameValuePair "x86_64-linux/${name}" (buildDerivationJob "build-x86_64-linux/${name}" "checks.x86_64-linux.${name}")
      )
    )
    // (
      (lib.removeAttrs config.flake.checks.aarch64-linux toRemove)
      |> lib.mapAttrs' (
        name: _:
        lib.nameValuePair "aarch64-linux/${name}" (
          buildDerivationJob "build-aarch64-linux/${name}" "checks.aarch64-linux.${name}"
        )
      )
    );
in
{
  perSystem =
    { config, pkgs, ... }:
    {
      checks = config.packages; # all the packages are also checks

      files.files = [
        {
          path_ = ".forgejo/workflows/build-checks.yaml";
          drv = pkgs.writers.writeJSON "forgejo-workflow-build-checks.yaml" {
            on = {
              push = { };
              workflow_dispatch = { };
            };
            jobs = buildJobs;
          };
        }
      ];
    };
}
