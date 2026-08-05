{ config, ... }:
{
  flake.modules.homeManager.vscode =
    { pkgs, ... }:
    {
      programs.vscode = {
        enable = true;
        # On darwin we install VS Code/Cursor via Homebrew cask and let HM
        # manage only the user config. On Linux we use vscodium from nixpkgs.
        package = if pkgs.stdenv.hostPlatform.isLinux then pkgs.vscodium else null;
      };
    };

  flake.modules.homeManager.workstation.imports = with config.flake.modules.homeManager; [
    vscode
  ];
}
