{ config, ... }:
{
  flake.modules.homeManager.vscode =
    { pkgs, lib, ... }:
    {
      # On Linux we use vscodium from nixpkgs, via its dedicated HM module so
      # config lands in VSCodium's own paths (plain programs.vscode warns).
      programs.vscodium.enable = pkgs.stdenv.hostPlatform.isLinux;
      # On darwin we install VS Code/Cursor via Homebrew cask and let HM
      # manage only the user config.
      programs.vscode = lib.mkIf pkgs.stdenv.hostPlatform.isDarwin {
        enable = true;
        package = null;
      };
    };

  flake.modules.homeManager.workstation.imports = with config.flake.modules.homeManager; [
    vscode
  ];
}
