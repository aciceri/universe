{
  flake.modules.nixos.base = {
    programs.nh = {
      enable = true;
      clean = {
        enable = true;
        extraArgs = "--keep-since 4d --keep 3";
      };
    };
  };

  flake.modules.homeManager.base =
    { config, ... }:
    {
      programs.nushell.environmentVariables.NH_FLAKE = config.universePath;
    };
}
