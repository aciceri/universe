{
  configurations.darwin.archer.module =
    { pkgs, ... }:
    {
      system.primaryUser = "ccr";
      users.users.ccr = {
        home = "/Users/ccr";
        shell = pkgs.nushell;
      };

      environment.variables = {
        NH_FLAKE = "/Users/ccr/universe";
        XDG_CONFIG_HOME = "/Users/ccr/.config";
      };

      # ctrl+cmd+drag anywhere on a window to move it, so you can rearrange
      # floating windows without aiming at the titlebar.
      system.defaults.NSGlobalDomain.NSWindowShouldDragOnGesture = true;
    };
}
