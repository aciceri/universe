{ inputs, ... }:
{
  flake.modules.homeManager.workstation =
    { pkgs, ... }:
    let
      spicePkgs = inputs.spicetify.legacyPackages.${pkgs.stdenv.system};
    in
    {
      imports = [ inputs.spicetify.homeManagerModules.spicetify ];

      programs.spicetify = {
        enable = true;
        enabledExtensions = with spicePkgs.extensions; [
          adblockify
        ];
      };
    };
}
