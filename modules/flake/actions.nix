{
  config,
  inputs,
  lib,
  ...
}:
{
  imports = [ inputs.actions-nix.flakeModules.default ];

  perSystem =
    { config, ... }:
    {
      checks = config.packages; # all the packages are also checks
    };

  flake.actions-nix =
    let
      buildDerivationJob = name: drvFlakePath: {
        steps = [
          {
            name = "Checkout repository";
            uses = "actions/checkout@v4";
          }
          {
            name = "Attic login";
            run = "attic login nixfleet http://sisko.wg.aciceri.dev:8081 $\{{secrets.ATTIC_NIXFLEET_TOKEN}}";
          }
          {
            name = "Build";
            run = ''
              nix build ".#${drvFlakePath}" -L
            '';
          }
          {
            name = "Push to Attic";
            run = "attic push nixfleet result";
          }
        ];
      };
      toRemove = [ "files/.gitignore" ];
      jobs =
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
      pre-commit.enable = true;
      defaultValues = {
        jobs.runs-on = "native";
      };
      workflows = {
        ".forgejo/workflows/main.yaml" = {
          inherit jobs;
        };
      };
    };
}
