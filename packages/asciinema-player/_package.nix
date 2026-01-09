{
  lib,
  buildNpmPackage,
  fetchFromGitHub,
  rustPlatform,
  lld,
  cargo,
  buildWasmBindgenCli,
  fetchCrate,
  makeWrapper,
  writeShellScript,
  nix-update,
}:
let
  wasm-bindgen-cli_0_2_92 = buildWasmBindgenCli rec {
    src = fetchCrate {
      pname = "wasm-bindgen-cli";
      version = "0.2.92";
      hash = "sha256-1VwY8vQy7soKEgbki4LD+v259751kKxSxmo/gqE6yV0=";
    };

    cargoDeps = rustPlatform.fetchCargoVendor {
      inherit src;
      inherit (src) pname version;
      hash = "sha256-81vQkKubMWaX0M3KAwpYgMA1zUQuImFGvh5yTW+rIAs=";
    };
  };

in
buildNpmPackage rec {
  pname = "asciinema-player";
  version = "3.14.0";

  src = fetchFromGitHub {
    owner = "asciinema";
    repo = "asciinema-player";
    rev = "v3.14.0";
    hash = "sha256-ZrF9Y4aXfIeIU4y145bYBJZ36LqI+jXimpXlL6cT+F8=";
  };

  npmDepsHash = "sha256-V0KmvY4MBgtXggJxRWuYglQyG2zd/kRH46H+boBYD5w=";

  cargoDeps = rustPlatform.fetchCargoVendor {
    inherit src;
    cargoRoot = "src/vt";
    hash = "sha256-anyrCXM84CfbTxNzaQSWbLY42rKM0KESwibvN1csfao=";
  };

  cargoRoot = "src/vt";

  nativeBuildInputs = [
    rustPlatform.cargoSetupHook
    lld
    wasm-bindgen-cli_0_2_92
    makeWrapper
  ];

  env = {
    CARGO_BIN = lib.getExe cargo;
    WASM_BINDGEN_BIN = lib.getExe wasm-bindgen-cli_0_2_92;
  };

  installPhase = ''
    runHook preInstall

    mkdir -p $out/lib/asciinema-player
    cp -r dist/* $out/lib/asciinema-player/

    runHook postInstall
  '';

  passthru.updateScript = writeShellScript "update-script.sh" "${lib.getExe nix-update} --flake asciinema-player";

  meta = with lib; {
    description = "A web player for terminal session recordings";
    homepage = "https://github.com/asciinema/asciinema-player";
    license = licenses.asl20;
    maintainers = [ maintainers.aciceri ];
    platforms = platforms.all;
  };
}
