{ lib, ... }:
{
  _module.args.getCurrentDir = curPos: curPos.file |> lib.strings.match "/nix/store/[^/]+-[^/]+/(.*)/[^/]+" |> lib.head;
}
