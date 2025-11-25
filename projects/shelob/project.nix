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
    }: let
    in
    {
      make-shells.shelob = {
        inputsFrom = [
          config.make-shells.default.finalPackage
          config.packages.shelob.env
        ];
        buildInputs = with pkgs.haskellPackages; [
          cabal-install
          haskell-language-server
          hlint
          ormolu
        ];
      };

      packages = {
        shelob = pkgs.haskellPackages.callCabal2nix "shelob" src { };
      };

      treefmt.programs = {
        ormolu = {
          enable = true;
          includes = [ "${currentDir}/**/*.hs" ];
        };
      };
    };
}
