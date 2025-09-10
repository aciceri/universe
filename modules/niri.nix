{ inputs, lib, ... }:
{
  flake.modules.nixos.pc =
    { pkgs, ... }:
    {
      imports = [ inputs.niri.nixosModules.niri ];
      programs.niri = {
        enable = true;
        package = pkgs.niri;
      };
      niri-flake.cache.enable = false;
      graphicalSessions.niri.exec = lib.getExe' pkgs.niri "niri-session";
    };

  flake.modules.homeManager.pc =
    { pkgs, config, ... }:
    {
      programs.niri = {
        package = pkgs.niri;
        settings = {
          binds = with config.lib.niri.actions; {
            "Mod+T".action.spawn = lib.getExe' pkgs.foot "footclient";
            "Mod+Shift+E".action = quit;
            "Mod+Ctrl+Shift+E".action = quit { skip-confirmation = true; };
          };
          hotkey-overlay = {
            skip-at-startup = true;
          };
        };
      };
    };
}
