{

  flake.modules.homeManager.pc =
    { pkgs, ... }:
    {
      programs.chromium = {
        enable = true;
        package = pkgs.google-chrome;
      };
    };
}
