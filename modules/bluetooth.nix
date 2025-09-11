fpArgs@{ lib, ... }:
{
  flake.modules.nixos.pc =
    { config, ... }:
    {
      services.blueman.enable = true;
      hardware.bluetooth = {
        enable = true;
        settings = {
          General = {
            Name = config.networking.hostName;
            ControllerMode = "dual";
            FastConnectable = "true";
            Experimental = "true";
          };
          Policy = {
            AutoEnable = "true";
          };
        };
      };

      users.users =
        fpArgs.config.users
        |> lib.mapAttrs (
          _: _: {
            extraGroups = [ "bluetooth" ];
          }
        );
    };
}
