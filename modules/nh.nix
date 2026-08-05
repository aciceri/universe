{
  flake.modules.homeManager.base =
    { config, ... }:
    {
      programs.nh = {
        enable = true;
        flake = config.universePath;
        clean = {
          enable = true;
          extraArgs = "--keep-since 4d --keep 3";
        };
      };

      home.sessionVariables.NH_FLAKE = config.universePath;
    };
}
