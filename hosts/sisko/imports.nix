{ config, ... }:
{
  configurations.nixos.sisko.module = {
    imports = with config.flake.modules.nixos; [
      base
      claude-code-overlay
    ];
    home-manager.sharedModules = with config.flake.modules.homeManager; [ base ];

    # Headless host: without a login session there is no `systemd --user` for
    # ccr, so home-manager's activation cannot reach the user bus and a deploy
    # fails at "reloading user units for ccr" even though the system activated
    # fine. Lingering keeps that manager up, which also finally runs the user
    # units this host declares (a `git fetch` every ~16 min and a weekly
    # `nh clean user --keep-since 4d --keep 3`, both user-profile scoped).
    users.users.ccr.linger = true;
  };
}
