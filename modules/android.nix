{ config, lib, ... }:
{
  flake.modules.nixos.workstation = {
    users.users =
      config.users
      |> lib.mapAttrs (
        _: _: {
          extraGroups = [ "adbusers" ];
        }
      );
  };

  flake.modules.homeManager.workstation =
    { pkgs, ... }:
    {
      home.packages = with pkgs; [
        reinstall-magisk-on-lineageos
        android-studio
        gradle
        android-tools
        android-studio-tools
      ];
    };
}
