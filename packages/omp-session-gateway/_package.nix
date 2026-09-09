# alphastorm/omp-session-gateway: a loopback Bun daemon that lists every live
# omp collab session (the patched omp publishes them over an authenticated unix
# socket) and hands view/control capabilities to a mobile PWA on tap.
#
# Upstream installs itself imperatively: `omp-gateway install` stages a
# content-addressed copy of the runtime under ~/.local/state and writes its own
# systemd user unit. Nothing on the `serve` path reads that staged runtime, so
# here the tree stays in the store and the unit comes from the NixOS module.
#
# The daemon resolves its static assets as `../../web/dist/` relative to
# apps/gateway/src/cli.ts, hence the whole workspace layout is installed
# verbatim instead of a single bundled script.
{
  lib,
  stdenvNoCC,
  fetchFromGitHub,
  bun,
  cacert,
  makeBinaryWrapper,
  # Extra origins appended to the collab client's CSP `connect-src`. Upstream
  # hardcodes the public relay only, so a self-hosted one is blocked by the
  # browser before the WebSocket is opened.
  extraRelayOrigins ? [ "wss://collab.sisko.wg.aciceri.dev" ],
}:
let
  version = "0.3.0-unstable-2026-09-09";

  src = fetchFromGitHub {
    owner = "alphastorm";
    repo = "omp-session-gateway";
    rev = "a025f69a808ecf050ef10f2ac2971a97a4d8d360";
    hash = "sha256-SAs0kBbJ0lSyuI3TSQUxRA8sXfr89zae5dgzB4dVWkM=";
  };

  # `bun install` needs the network, so dependencies are a fixed-output
  # derivation. Bun's isolated install writes one node_modules per workspace
  # (root, apps/gateway, apps/web, packages/collab-client) with only relative
  # symlinks between them, so the trees are relocatable and byte-identical
  # across runs.
  nodeModules = stdenvNoCC.mkDerivation {
    pname = "omp-session-gateway-node-modules";
    inherit version src;

    nativeBuildInputs = [ bun ];
    dontConfigure = true;

    buildPhase = ''
      runHook preBuild
      export HOME="$NIX_BUILD_TOP/home"
      bun install --frozen-lockfile --production --no-progress --ignore-scripts
      runHook postBuild
    '';
    installPhase = ''
      runHook preInstall
      find . -maxdepth 3 -name node_modules -prune -print0 \
        | while IFS= read -r -d "" dir; do
            target="$out/''${dir#./}"
            mkdir -p "$(dirname "$target")"
            cp -R "$dir" "$target"
          done
      runHook postInstall
    '';

    dontFixup = true;

    outputHashAlgo = "sha256";
    outputHashMode = "recursive";
    outputHash = "sha256-ZMQo1/2skD5OwuqoK45HBS2ZyZmt104Ll2DrBwNs5Es=";

    impureEnvVars = lib.fetchers.proxyImpureEnvVars;
    env.SSL_CERT_FILE = "${cacert}/etc/ssl/certs/ca-bundle.crt";
  };
in
stdenvNoCC.mkDerivation {
  pname = "omp-session-gateway";
  inherit version src;

  nativeBuildInputs = [
    bun
    makeBinaryWrapper
  ];

  postPatch = lib.optionalString (extraRelayOrigins != [ ]) ''
    substituteInPlace apps/gateway/src/http.ts \
      --replace-fail "connect-src 'self' wss://my.omp.sh;" \
        "connect-src 'self' wss://my.omp.sh ${lib.concatStringsSep " " extraRelayOrigins};"
  '';

  configurePhase = ''
    runHook preConfigure
    cp -R --no-preserve=mode,ownership ${nodeModules}/. .
    runHook postConfigure
  '';

  buildPhase = ''
    runHook preBuild
    export HOME="$NIX_BUILD_TOP/home"
    bun run build
    runHook postBuild
  '';

  installPhase = ''
    runHook preInstall

    root=$out/libexec/omp-session-gateway
    mkdir -p "$root"
    cp -R \
      apps packages node_modules patches licenses schemas \
      package.json bun.lock bunfig.toml tsconfig.json \
      UPSTREAM.lock.json STABLE_RELEASE.lock.json \
      LICENSE NOTICE.md THIRD_PARTY_NOTICES.md \
      "$root/"

    # The daemon derives its asset root from the CLI's own location, so both
    # entry points must run the in-store cli.ts. `omp-gatewayd` upstream infers
    # `serve` from argv[0], which a wrapper would hide, so pass it explicitly.
    makeWrapper ${lib.getExe bun} $out/bin/omp-gateway \
      --add-flags "$root/apps/gateway/src/cli.ts"
    makeWrapper ${lib.getExe bun} $out/bin/omp-gatewayd \
      --add-flags "$root/apps/gateway/src/cli.ts serve"

    runHook postInstall
  '';

  passthru = {
    inherit src;
    # Applied to omp itself (see modules/agents.nix): stock omp neither
    # auto-starts collab nor publishes to the gateway registry. Not a plain
    # `patches` entry — it is a four-commit mbox that GNU patch mis-applies, so
    # the consumer runs `git apply`.
    ompPatch = "${src}/patches/oh-my-pi/0001-collab-controller-autostart-registry.patch";
  };

  meta = {
    description = "Private mobile directory and capability broker for live omp collab sessions";
    homepage = "https://github.com/alphastorm/omp-session-gateway";
    license = lib.licenses.mit;
    maintainers = [ lib.maintainers.aciceri ];
    mainProgram = "omp-gateway";
    platforms = lib.platforms.unix;
  };
}
