{
  claude-code,
  fetchzip,
  lib,
}:
lib.traceIf ((lib.strings.compareVersions claude-code.version "2.0.1") != -1)
  "ATTENTION: claude-code 2.0.1 or superior is now in nixpkgs, you can stop using this package"
  (
    claude-code.overrideAttrs (
      finalAttrs: previousAttrs: {
        pname = "yt-dlp-master";
        version = "2.0.1";
        src = fetchzip {
          url = "https://registry.npmjs.org/@anthropic-ai/claude-code/-/claude-code-${finalAttrs.version}.tgz";
          hash = "sha256-LUbDPFa0lY74MBU4hvmYVntt6hVZy6UUZFN0iB4Eno8=";
        };
        npmDepsHash = "sha256-DehkeMZvzn+hvcCDzJfd4p9oYc1GSZm8gu8vKS4Uncw=";
        meta.maintainers = previousAttrs.meta.maintainers ++ [ lib.maintainers.aciceri ];
      }
    )
  )
