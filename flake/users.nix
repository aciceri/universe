{ lib, inputs, ... }:
let
  userSubmoduleType = lib.types.submodule (
    { name, ... }:
    {
      options = {
        name = lib.mkOption {
          type = lib.types.str;
          default = name;
        };
        sshKeys = lib.mkOption {
          type = lib.types.listOf lib.types.str;
          default =
            let
              keys = inputs."ghkeys-${name}" or null;
            in
            if builtins.isNull keys then
              [ ]
            else
              lib.splitString "\n" (builtins.readFile keys) |> lib.unique |> lib.filter (k: k != "");
        };
        god = lib.mkOption {
          type = lib.types.bool;
          default = false;
        };
      };
    }
  );
in
{
  options.users = lib.mkOption {
    type = lib.types.attrsOf userSubmoduleType;
  };

  config = {
    users = {
      ccr.god = true;
    };
  };
}
