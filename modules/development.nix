{
  flake.modules.homeManager.workstation =
    { pkgs, ... }:
    {
      home.packages = with pkgs; [
        nixd
        nixfmt
        python3Packages.python-lsp-server
        vtsls
        devenv
        nixpkgs-review
        nurl
        nix-fast-build
      ];

      programs.gh = {
        enable = true;
        settings = {
          git_protocol = "ssh";
          prompt = "enabled";
        };
      };

      programs.gh-dash.enable = true;
    };
}
