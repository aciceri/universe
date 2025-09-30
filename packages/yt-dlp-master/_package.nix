{
  yt-dlp,
  fetchFromGitHub,
  lib,
  nix-update-script,
}:
yt-dlp.overrideAttrs (
  finalAttrs: previousAttrs: {
    pname = "yt-dlp-master";
    version = "2025.09.26";
    src = fetchFromGitHub {
      owner = "yt-dlp";
      repo = "yt-dlp";
      rev = "4429fd0450a3fbd5e89573e06533c1a0874fae42";
      hash = "sha256-/uzs87Vw+aDNfIJVLOx3C8RyZvWLqjggmnjrOvUX1Ea=";
    };
    passthru.updateScript = nix-update-script {
      extraArgs = [ "--version=branch" ];
    };
    meta.maintainers = previousAttrs.meta.maintainers ++ [ lib.maintainers.aciceri ];
  }
)
