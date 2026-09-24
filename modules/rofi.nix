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
        settings = {
          modi = "drun,window,ssh";
          combi-modes = [
            "drun"
            "window"
            "ssh"
          ];
          terminal = lib.getExe config.programs.alacritty.package;
        };
        theme = {
          window.padding = mkLiteral "1ch";
          inputbar.spacing = mkLiteral "1ch";
        };
        plugins = [ pkgs.rofi-calc ];
      };
    };
}
