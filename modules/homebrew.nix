{ inputs, ... }:
{
  flake.modules.darwin.homebrew =
    { config, ... }:
    {
      imports = [ inputs.nix-homebrew.darwinModules.nix-homebrew ];

      nix-homebrew = {
        enable = true;
        enableRosetta = true;
        user = config.system.primaryUser;
        taps = {
          "homebrew/homebrew-core" = inputs.homebrew-core;
          "homebrew/homebrew-cask" = inputs.homebrew-cask;
          "homebrew/homebrew-bundle" = inputs.homebrew-bundle;
          "BarutSRB/homebrew-tap" = inputs.homebrew-barutsrb-tap;
          "startergo/homebrew-qemu-virgl-kosmickrisp" = inputs.homebrew-startergo-qemu-virgl;
          "startergo/homebrew-virglrenderer" = inputs.homebrew-startergo-virglrenderer;
          "startergo/homebrew-libepoxy" = inputs.homebrew-startergo-libepoxy;
          "startergo/homebrew-angle" = inputs.homebrew-startergo-angle;
          "startergo/homebrew-gn" = inputs.homebrew-startergo-gn;
        };
        mutableTaps = false;
        # Homebrew 6 requires explicit trust for non-official taps. During
        # activation `brew bundle` runs under sudo with XDG_CONFIG_HOME stripped,
        # so a manual `brew trust` (written to ~/.config/homebrew/trust.json) is
        # invisible to it — it reads ~/.homebrew/trust.json instead. nix-homebrew
        # writes the trust entries via the same env, so declare them here. These
        # taps are flake-pinned (mutableTaps = false), so trusting them wholesale
        # is bounded to the locked revisions.
        trust.taps = [
          "BarutSRB/tap"
          "startergo/qemu-virgl-kosmickrisp"
          "startergo/virglrenderer"
          "startergo/libepoxy"
          "startergo/angle"
          "startergo/gn"
        ];
      };

      homebrew = {
        enable = true;
        onActivation = {
          # No autoUpdate: taps are flake-pinned (mutableTaps = false), so
          # `brew update` has nothing legitimate to do here.
          autoUpdate = false;
          cleanup = "uninstall";
          upgrade = true;
        };
        taps = [
          "homebrew/core"
          "homebrew/cask"
          "homebrew/bundle"
          "BarutSRB/tap"
          "startergo/qemu-virgl-kosmickrisp"
          "startergo/virglrenderer"
          "startergo/libepoxy"
          "startergo/angle"
          "startergo/gn"
        ];
      };
    };
}
