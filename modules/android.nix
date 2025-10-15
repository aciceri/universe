{ config, lib, ... }:
{
  flake.modules.nixos.workstation =
    { pkgs, ... }:
    {
      programs.adb.enable = true;

      users.users =
        config.users
        |> lib.mapAttrs (
          _: _: {
            extraGroups = [ "adbusers" ];
          }
        );

      services.udev.packages = [
        pkgs.android-udev-rules
      ];
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
