{ lib, ... }:
{
  flake.modules.homeManager.pc =
    { config, pkgs, ... }:
    let
      inherit (config.lib.formats.rasi) mkLiteral;
    in
    {
      programs.rofi = {
        enable = true;
        extraConfig = {
          modi = "drun,window,ssh";
          combi-modes = [
            "drun"
            "window"
            "ssh"
          ];
        };
        theme = {
          window.padding = mkLiteral "1ch";
          inputbar.spacing = mkLiteral "1ch";
        };
        terminal = lib.getExe' pkgs.foot "footclient";
        pass.enable = true;
        plugins = [ pkgs.rofi-calc ];
      };
    };
}
