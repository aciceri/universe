{ getCurrentDir, ... }:
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

  readme.parts.projects = ''
    ### CalTrack

    Simple Python tool to extract billable hours from my calendar.
    In a future it may be extended to directly create invoices.
  '';
}
