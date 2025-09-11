{
  flake.modules.homeManager.workstation =
    { pkgs, ... }:
    {
      home.packages = with pkgs; [
        nixd
        nixfmt
        claude-code
        python3Packages.python-lsp-server
        vtsls
        devenv
      ];
    };
}
