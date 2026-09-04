{
  # Blender plus its MCP bridge. blender-mcp is two halves that speak JSON
  # over TCP (127.0.0.1:9876): the MCP server (`pkgs.blender-mcp`, spawned by
  # the MCP client) connects *into* a socket server that lives inside Blender
  # as an add-on. Both halves carry a protocol version and refuse to cooperate
  # when they drift, so the add-on is symlinked straight out of the package
  # instead of being copied into $HOME by `blender-mcp install-addon` — one
  # closure, one version, no stale file.
  #
  # The MCP client side stays outside Nix: omp keeps its server list in the
  # mutable ~/.omp/agent/mcp.json (it writes OAuth material back into it), so
  # the `blender` entry there is hand-managed:
  #   { "command": "blender-mcp", "env": { "DISABLE_TELEMETRY": "true" } }
  flake.modules.homeManager.workstation =
    { pkgs, lib, ... }:
    {
      home.packages = [
        pkgs.blender
        pkgs.blender-mcp
      ];

      # Blender loads user add-ons only from the directory of the exact version
      # it is, hence deriving the path from pkgs.blender instead of pinning it.
      xdg.configFile."blender/${lib.versions.majorMinor pkgs.blender.version}/scripts/addons/blender_mcp.py".source =
        "${pkgs.blender-mcp}/share/blender-mcp/addon.py";
    };
}
