{ writers, python3Packages }:
writers.writePython3Bin "mirror-checks" {
  libraries = with python3Packages; [
    requests
    pygithub
  ];
} (builtins.readFile ./mirror-checks.py)
