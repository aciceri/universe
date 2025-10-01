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
        run = ''nix build ${lib.escapeShellArg ".#${drvFlakePath}"} -L'';
      }
      {
        name = "Push to Attic";
        run = "attic push sisko result";
      }
    ];
  };

  syncToGitHubJob = {
    runs-on = "native";
    needs = lib.attrNames buildJobs;
    "if" = "always()";
    steps = [
      {
        name = "Checkout repository";
        uses = "actions/checkout@v4";
      }
      {
        name = "Sync checks to GitHub";
        env = {
          GITHUB_TOKEN = "$\{{secrets.ACICERI_GITHUB_TOKEN}}";
          FORGEJO_URL = "https://git.aciceri.dev";
        };
        run = "nix run .#mirror-checks -- --repo $\{{forge.repository}} --commit $\{{forge.sha}}";
      }
    ];
  };

  callMergePrJob = {
    runs-on = "native";
    needs = lib.attrNames buildJobs;
    "if" =
      "$\{{ success() && (github.ref == 'refs/heads/update-flake-lock' || github.ref == 'refs/heads/nix-update-script') }}";
    uses = "./.forgejo/workflows/merge-pr.yaml";
    secrets = {
      SEVENOFNINE_TOKEN = "$\{{secrets.SEVENOFNINE_TOKEN}}";
    };
  };

  buildJobs =
    (
      config.flake.checks.x86_64-linux
      |> lib.mapAttrs' (
        name: _:
        lib.nameValuePair "x86_64-linux/${name}" (
          buildDerivationJob "build-x86_64-linux/${name}" ''checks.x86_64-linux."${name}"''
        )
      )
    )
    // (
      config.flake.checks.aarch64-linux
      |> lib.mapAttrs' (
        name: _:
        lib.nameValuePair "aarch64-linux/${name}" (
          buildDerivationJob "build-aarch64-linux/${name}" ''checks.aarch64-linux."${name}"''
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
            name = "Build flake checks";
            on = {
              push = { };
              workflow_dispatch = { };
            };
            jobs = buildJobs // {
              mirror-checks = syncToGitHubJob;
              call-merge-pr = callMergePrJob;
            };
          };
        }
      ];
    };
}
