{ lib, ... }:
{
  flake.modules.homeManager.pc =
    { config, pkgs, ... }:
    {
      programs.alacritty = {
        enable = true;
        settings = {
          mouse.hide_when_typing = true;
          keyboard.bindings = [
            {
              key = "n";
              mods = "Control|Shift";
              action = "SpawnNewInstance";
            }
            {
              key = "g";
              mods = "Control|Shift";
              command =
                pkgs.writeShellScriptBin "test" ''
                  alacritty --title "lazyGit" -e ${lib.getExe config.programs.lazygit.package}
                ''
                |> lib.getExe;
            }
            # To make Shift+Enter work in claude-code
            {
              key = "Return";
              mods = "Shift";
              chars = "\n";
            }
          ];
        };
      };
    };
}
