{
  configurations.darwin.archer.module =
    {
      config,
      lib,
      pkgs,
      ...
    }:
    {
      system.primaryUser = "ccr";
      users.users.ccr = {
        home = "/Users/ccr";
        shell = pkgs.nushell;
      };

      environment.variables = {
        NH_FLAKE = lib.mkIf config.home-manager.users.ccr.multiverse.enable config.home-manager.users.ccr.universePath;
        XDG_CONFIG_HOME = "/Users/ccr/.config";
      };

      # ctrl+cmd+drag anywhere on a window to move it, so you can rearrange
      # floating windows without aiming at the titlebar.
      system.defaults.NSGlobalDomain.NSWindowShouldDragOnGesture = true;
    };
}
