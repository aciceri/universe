{
  flake.modules.homeManager.workstation =
    { config, pkgs, ... }:
    let
      terraform-ts-mode = pkgs.emacsPackages.trivialBuild {
        pname = "terraform-ts-mode";
        version = "0.1.0";
        src = pkgs.fetchFromGitHub {
          owner = "kgrotel";
          repo = "terraform-ts-mode";
          rev = "master";
          hash = "sha256-CzxH3pXPsMASCKpX6lxm2TNkl5GNKhtOJ34U+71wV8E=";
        };
        packageRequires = [ ];
      };
    in
    {
      home.packages = with pkgs; [
        claude-code-acp
        ruff
        nixd
        vtsls
        terraform-ls
      ];

      home.file.".config/emacs/init.el".source =
        config.lib.file.mkOutOfStoreSymlink "/home/ccr/universe/modules/emacs/init.el";
      programs.emacs = {
        enable = true;
        package = pkgs.emacs-pgtk;
        extraPackages =
          epkgs: with epkgs; [
            helix
            consult
            consult-project-extra
            consult-eglot
            vertico
            multiple-cursors
            corfu
            nix-ts-mode
            haskell-ts-mode
            agent-shell
            orderless
            marginalia
            god-mode
            flyover
            indent-bars
            treesit-grammars.with-all-grammars
            terraform-ts-mode
            catppuccin-theme
            rainbow-delimiters
          ];
      };
    };
}
