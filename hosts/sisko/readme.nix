{ config, lib, ... }:
let
  virtualHosts =
    config.flake.nixosConfigurations.sisko.config.services.nginx.virtualHosts
    |> lib.mapAttrsToList (name: _: name)
    |> lib.filter (name: name != "localhost")
    |> lib.naturalSort
    |> lib.partition (lib.hasSuffix "wg.aciceri.dev")
    |> (attrs: {
      wireguard = attrs.right;
      public = attrs.wrong;
    });

  mkMarkdownList = lib.concatMapStringsSep "\n" (s: "- [${s}](https://${s})");

  wireguardVirtualHosts = mkMarkdownList virtualHosts.wireguard;
  publicVirtualHosts = mkMarkdownList virtualHosts.public;
in
{
  # TODO expand
  readme.parts.nixos = ''
    ### Sisko

    My homeserver hosting a wide variety of private and public services.
    Running on my decommissioned Thinkpad Carbon X1 gen 7

    #### Web services

    ##### Public
    ${publicVirtualHosts}

    ##### WireGuard
    ${wireguardVirtualHosts}
  '';
}
