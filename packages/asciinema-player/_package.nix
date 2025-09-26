{
  lib,
  buildNpmPackage,
  fetchFromGitHub,
  rustPlatform,
  lld,
  wasm-bindgen-cli_0_2_92,
  cargo,
  makeWrapper,
}:

buildNpmPackage rec {
  pname = "asciinema-player";
  version = "3.10.0";

  src = fetchFromGitHub {
    owner = "asciinema";
    repo = "asciinema-player";
    rev = "v3.10.0";
    hash = "sha256-4fL3sxJthk25Hg00a04EJ76JbQEXeyFDPfz9cet5RjA=";
  };

  npmDepsHash = "sha256-InEp8tYvOFRKD60uTtuqJ+mXGj2nUfDR8IwVOU4ibe8=";

  cargoDeps = rustPlatform.fetchCargoVendor {
    inherit src;
    cargoRoot = "src/vt";
    hash = "sha256-smo16zoIAmwI/87V6h/5ygY088C0dRKU5guGitXB/8Q=";
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

  meta = with lib; {
    description = "A web player for terminal session recordings";
    homepage = "https://github.com/asciinema/asciinema-player";
    license = licenses.asl20;
    maintainers = [ maintainers.aciceri ];
    platforms = platforms.all;
    sourceProvenance = with sourceTypes; [ fromSource ];
  };
}
