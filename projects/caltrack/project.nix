{ withSystem, getCurrentDir, ... }:
let
  currentDir = getCurrentDir __curPos;
in
{
  perSystem =
    {
      config,
      pkgs,
      ...
    }:
    let
      deps = with pkgs.python3Packages; [
        dateparser
        icalendar
        requests
        rich
      ];
    in
    {
      make-shells.caltrack = {
        inputsFrom = [
          config.make-shells.default.finalPackage
        ];
        buildInputs = [
          pkgs.python3
          config.packages.caltrack
        ]
        ++ deps;
      };

      packages = {
        caltrack = pkgs.writers.writePython3Bin "caltrack" {
          libraries = deps;
        } (builtins.readFile ./caltrack.py);
      };

      treefmt.programs = {
        ruff-format = {
          enable = true;
          includes = [ "${currentDir}/**/*.py" ];
        };
        ruff-check = {
          enable = true;
          includes = [ "${currentDir}/**/*.py" ];
        };
      };
    };

  flake.modules.nixos.workstation = {
    secrets.caltrack_url.owner = "ccr";
  };

  flake.modules.homeManager.workstation =
    { osConfig, pkgs, ... }:
    {
      home.packages = withSystem (pkgs.stdenv.system) (
        { config, ... }:
        [
          config.packages.caltrack
        ]
      );

      programs.nushell.extraConfig = ''
        $env.CALTRACK_URL = (open ${osConfig.age.secrets.caltrack_url.path} | str trim)
      '';
    };

  readme.parts.projects = ''
    ### CalTrack

    Simple Python tool to extract billable hours from my calendar.
    In a future it may be extended to directly create invoices.
  '';
}
