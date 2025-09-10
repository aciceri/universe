{ lib, ... }:
{
  flake.modules.nixos.pc =
    { config, pkgs, ... }:
    let
      sessions =
        config.graphicalSessions
        |> lib.attrValues
        |> lib.map (
          session:
          pkgs.writeTextFile {
            name = "${session.name}-session";
            destination = "/${session.name}-session.desktop";
            text = ''
              [Desktop Entry]
              Name=${session.name}
              Exec=${session.exec}
            '';
          }
        )
        |> lib.concatStringsSep ":";
    in
    {
      options.graphicalSessions = lib.mkOption {
        type = lib.types.attrsOf (
          lib.types.submodule (
            { name, ... }:
            {
              options = {
                name = lib.mkOption {
                  type = lib.types.str;
                  default = name;
                };
                exec = lib.mkOption {
                  type = lib.types.str;
                };
              };
              config = { };
            }
          )
        );
        default = { };
      };
      config.services.greetd = {
        enable = true;
        settings = {
          default_session = {
            command = lib.concatStringsSep " " [
              (lib.getExe pkgs.tuigreet)
              "--time"
              "--remember"
              "--remember-user-session"
              "--asterisks"
              "--sessions '${sessions}'"
            ];
            user = "greeter";
          };
        };
      };
    };
}
