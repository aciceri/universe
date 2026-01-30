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
        gitignore.follows = "";
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
    nix-ai-tools = {
      url = "github:numtide/nix-ai-tools";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        treefmt-nix.follows = "treefmt-nix";
        blueprint.follows = "blueprint_";
      };
    };
    nixpkgs-amule.url = "github:aciceri/nixpkgs/fix-amule-module";
    nuschtos = {
      # TODO use
      url = "github:NuschtOS/search";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        flake-utils.follows = "flake-utils_";
      };
    };
    nixpkgs-influxdb.url = "github:aciceri/nixpkgs/fix-libflux";
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
    flake-compat_ = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
    crane_.url = "github:ipetkov/crane";
    flake-utils_ = {
      url = "github:numtide/flake-utils";
      inputs.systems.follows = "nix-systems_";
    };
    nixpkgs-stable_.url = "github:NixOS/nixpkgs/nixos-25.05";
    blueprint_ = {
      url = "github:numtide/blueprint";
      inputs.systems.follows = "nix-systems_";
    };
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
