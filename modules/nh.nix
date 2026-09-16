{
  flake.modules.homeManager.base =
    { config, lib, ... }:
    {
      programs.nh = {
        enable = true;
        flake = lib.mkIf config.multiverse.enable config.universePath;
        clean = {
          enable = true;
          extraArgs = "--keep-since 4d --keep 3";
        };
      };

      home.sessionVariables = lib.mkIf config.multiverse.enable {
        NH_FLAKE = config.universePath;
      };
      # home.sessionVariables only reaches POSIX shells via hm-session-vars.sh;
      # nushell (the login shell) needs the variable delivered explicitly.
      programs.nushell.environmentVariables = lib.mkIf config.multiverse.enable {
        NH_FLAKE = config.universePath;
      };
    };
}
