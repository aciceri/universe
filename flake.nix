{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";

    treefmt-nix = {
      url = "github:numtide/treefmt-nix";
      inputs.nixpkgs.follows = "";
    };
    git-hooks = {
      url = "github:cachix/git-hooks.nix";
      inputs = {
        nixpkgs.follows = "";
        flake-compat.follows = "";
      };
    };
    make-shell = {
      url = "github:nicknovitski/make-shell";
      inputs.flake-compat.follows = "";
    };
    agenix-shell = {
      url = "github:aciceri/agenix-shell";
      inputs = {
        flake-parts.follows = "flake-parts";
        nixpkgs.follows = "";
        flake-root.follows = "flake-root_";
        treefmt-nix.follows = "treefmt-nix";
        git-hooks-nix.follows = "git-hooks";
        nix-github-actions.follows = "";
      };
    };

    agenix = {
      url = "github:ryantm/agenix";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        home-manager.follows = "";
        systems.follows = "nix-systems_";
      };
    };
    nixos-facter-modules.url = "github:nix-community/nixos-facter-modules";
    disko = {
      url = "github:nix-community/disko";
      inputs.nixpkgs.follows = "";
    };
    lanzaboote = {
      url = "github:nix-community/lanzaboote";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        pre-commit.follows = "";
        crane.follows = "crane_";
        rust-overlay.follows = "rust-overlay_";
      };
    };
    impermanence.url = "github:nix-community/impermanence";
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nix-darwin = {
      url = "github:nix-darwin/nix-darwin";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nix-homebrew.url = "github:zhaofengli/nix-homebrew";
    homebrew-core = {
      url = "github:homebrew/homebrew-core";
      flake = false;
    };
    homebrew-cask = {
      url = "github:homebrew/homebrew-cask";
      flake = false;
    };
    homebrew-bundle = {
      url = "github:homebrew/homebrew-bundle";
      flake = false;
    };
    homebrew-barutsrb-tap = {
      url = "github:BarutSRB/homebrew-tap";
      flake = false;
    };
    stylix = {
      url = "github:nix-community/stylix";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        systems.follows = "nix-systems_";
        nur.follows = "nur";
      };
    };
    nur = {
      url = "github:nix-community/nur";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        flake-parts.follows = "flake-parts";
      };
    };
    niri = {
      url = "github:sodiboo/niri-flake";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        nixpkgs-stable.follows = "nixpkgs-stable_";
        niri-stable.follows = "";
        niri-unstable.follows = "";
        xwayland-satellite-stable.follows = "";
        xwayland-satellite-unstable.follows = "";
      };
    };
    dms = {
      url = "github:AvengeMedia/DankMaterialShell";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    zen-browser = {
      url = "github:0xc000022070/zen-browser-flake";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        home-manager.follows = "home-manager";
      };
    };
    claude-desktop = {
      url = "github:k3d3/claude-desktop-linux-flake";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        flake-utils.follows = "flake-utils_";
      };
    };
    spicetify = {
      url = "github:Gerg-L/spicetify-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    # Pinned to the 0.10.0 rc line until nixpkgs ships ncps >= 0.10.0
    # (0.9.x 500s on opaque NAR URLs, kalbasit/ncps#1331) — see modules/ncps.nix.
    ncps = {
      url = "github:kalbasit/ncps/v0.10.0-rc16";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        flake-parts.follows = "flake-parts";
        treefmt-nix.follows = "treefmt-nix";
        git-hooks-nix.follows = "git-hooks";
      };
    };
    llm-agents = {
      url = "github:numtide/llm-agents.nix";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        systems.follows = "nix-systems_";
        flake-parts.follows = "flake-parts";
      };
    };
    meridian = {
      url = "github:rynfar/meridian";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        systems.follows = "nix-systems_";
      };
    };
  };

  # Grab SSH keys from GitHub
  inputs = {
    ghkeys-ccr = {
      url = "https://github.com/aciceri.keys";
      flake = false;
    };
  };

  # For deduplication
  inputs = {
    flake-root_.url = "github:srid/flake-root";
    nix-systems_.url = "github:nix-systems/default";
    rust-overlay_ = {
      url = "github:oxalica/rust-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    homebrew-startergo-qemu-virgl = {
      url = "github:startergo/homebrew-qemu-virgl-kosmickrisp";
      flake = false;
    };
    homebrew-startergo-virglrenderer = {
      url = "github:startergo/homebrew-virglrenderer";
      flake = false;
    };
    homebrew-startergo-libepoxy = {
      url = "github:startergo/homebrew-libepoxy";
      flake = false;
    };
    homebrew-startergo-angle = {
      url = "github:startergo/homebrew-angle";
      flake = false;
    };
    homebrew-startergo-gn = {
      url = "github:startergo/homebrew-gn";
      flake = false;
    };
    crane_.url = "github:ipetkov/crane";
    flake-utils_ = {
      url = "github:numtide/flake-utils";
      inputs.systems.follows = "nix-systems_";
    };
    nixpkgs-stable_.url = "github:NixOS/nixpkgs/nixos-25.05";
  };

  nixConfig.allow-import-from-derivation = true;

  outputs =
    inputs:
    inputs.flake-parts.lib.mkFlake { inherit inputs; } (
      { lib, ... }:
      {
        imports =
          lib.filesystem.listFilesRecursive ./.
          |> lib.map toString
          |> lib.filter (lib.hasSuffix ".nix")
          |> lib.filter (f: !lib.hasSuffix "flake.nix" f)
          |> lib.filter (f: !lib.hasInfix "/_" f);

        _module.args.rootPath = ./.;
      }
    );
}
