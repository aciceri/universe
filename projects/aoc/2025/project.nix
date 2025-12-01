{ lib, getCurrentDir, ... }:
let
  currentDir = getCurrentDir __curPos;
  src = lib.cleanSourceWith {
    src = ./.;
    filter = path: _: !(path |> lib.hasSuffix ".nix");
  };
in
{
  gitignore =
    [
      "_cache"
      "out"
      "dist-newstyle"
    ]
    |> lib.map (path: "${currentDir}/${path}");

  perSystem =
    {
      config,
      pkgs,
      ...
    }:
    {
      make-shells.aoc-2025 = {
        inputsFrom = [
          config.make-shells.default.finalPackage
          config.packages.aoc-2025.env
        ];
        buildInputs = with config.packages.aoc-2025.scope; [
          cabal-install
          haskell-language-server
          hlint
          ormolu
        ];
      };

      packages = {
        # Using GHC 9.6 because HLS crashes when using megaparsec otherwise
        aoc-2025 = pkgs.haskell.packages.ghc96.callCabal2nix "aoc-2025" src { };
      };

      treefmt.programs = {
        ormolu = {
          enable = true;
          includes = [ "${currentDir}/**/*.hs" ];
        };
      };
    };
}
