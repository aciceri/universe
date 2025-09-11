{ lib, ... }:
{
  flake.modules.homeManager.pc =
    { pkgs, ... }:
    {
      programs.rofi = {
        enable = true;
        package = pkgs.rofi-wayland;
        extraConfig = {
          modi = "drun,window,ssh";
          combi-modes = [
            "drun"
            "window"
            "ssh"
          ];
        };
        terminal = lib.getExe' pkgs.foot "footclient";
        pass = {
          enable = true;
          package = pkgs.rofi-pass-wayland;
        };
        plugins = [ pkgs.rofi-calc ];
      };
    };
}
