{
  lib,
  python3Packages,
  fetchPypi,
  nix-update,
  writeShellScript,
}:
python3Packages.buildPythonApplication rec {
  pname = "blender-mcp";
  version = "1.9.1";
  pyproject = true;

  src = fetchPypi {
    pname = "blender_mcp";
    inherit version;
    hash = "sha256-EIu5TXRUllajUtl+m+54TPgQ2J526c7YdPM3KQHx3hU=";
  };

  build-system = with python3Packages; [ setuptools ];

  dependencies = with python3Packages; [
    mcp
    httpx
  ];

  # The PyPI sdist ships tests/ but not tests/conftest.py, so every test
  # module fails at collection on `import conftest`. Upstream publishes no
  # git tags, so there is no equivalent tagged source to build from instead.
  doCheck = false;

  pythonImportsCheck = [ "blender_mcp.server" ];

  # The Blender side is a plain add-on file that must live in Blender's own
  # scripts/addons directory. Exposing the copy bundled with *this* version
  # keeps the socket protocol version of add-on and server in lockstep, which
  # `blender-mcp install-addon` cannot guarantee (it writes into $HOME).
  postInstall = ''
    install -Dm444 src/blender_mcp/bundled/addon.py $out/share/blender-mcp/addon.py
  '';

  passthru.updateScript = writeShellScript "update-script.sh" "${lib.getExe nix-update} --flake blender-mcp";

  meta = {
    description = "MCP server driving Blender through a socket add-on";
    longDescription = ''
      Third-party MCP server that lets any MCP client inspect and edit a running
      Blender scene. It talks JSON over TCP (default port 9876) to the companion
      Blender add-on, shipped here as `share/blender-mcp/addon.py`.
    '';
    homepage = "https://github.com/ahujasid/blender-mcp";
    license = lib.licenses.mit;
    mainProgram = "blender-mcp";
    platforms = lib.platforms.all;
  };
}
