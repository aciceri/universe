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
            KernelExperimental = "true";
          };
          Policy = {
            AutoEnable = "true";
            ReconnectAttempts = "7";
            ReconnectIntervals = "1,2,4,8,16,32,64";
          };
        };
        powerOnBoot = true;
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
