{
  yt-dlp,
  fetchFromGitHub,
  lib,
}:
yt-dlp.overrideAttrs (
  finalAttrs: previousAttrs: {
    version = "2025.09.26";
    src = fetchFromGitHub {
      owner = "yt-dlp";
      repo = "yt-dlp";
      tag = finalAttrs.version;
      hash = "sha256-/uzs87Vw+aDNfIJVLOx3C8RyZvWLqjggmnjrOvUX1Eg=";
    };
    meta.maintainers = previousAttrs.meta.maintainers ++ [ lib.maintainers.aciceri ];
  }
)
