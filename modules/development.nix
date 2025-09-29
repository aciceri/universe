{
  flake.modules.homeManager.workstation =
    { pkgs, ... }:
    {
      home.packages = with pkgs; [
        nixd
        nixfmt
        claude-code
        gemini-cli
        python3Packages.python-lsp-server
        vtsls
        devenv
        nixpkgs-review
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
