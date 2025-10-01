{
  yt-dlp,
  fetchFromGitHub,
  lib,
  nix-update-script,
}:
yt-dlp.overrideAttrs (
  finalAttrs: previousAttrs: {
    pname = "yt-dlp-master";
    version = "unstable-2025-09-29";
    src = fetchFromGitHub {
      owner = "yt-dlp";
      repo = "yt-dlp";
      rev = "bd5ed90419eea18adfb2f0d8efa9d22b2029119f";
      hash = "sha256-1x44tnkHN+KQpvwrm+t61eNm7e2+El1IPHvgBYPufDI=";
    };
    passthru = previousAttrs.passthru // {
      updateScript = nix-update-script {
        extraArgs = [ "--version=branch" ];
      };
    };
    meta.maintainers = previousAttrs.meta.maintainers ++ [ lib.maintainers.aciceri ];
  }
)
