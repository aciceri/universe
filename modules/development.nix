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
        nix-update
        nix-melt
        nix-tree
        google-cloud-sdk
      ];

      programs.gh = {
        enable = true;
        settings = {
          git_protocol = "ssh";
          prompt = "enabled";
        };
      };

      programs.gh-dash.enable = true;

      programs.nushell.extraConfig = ''
        def nixpkgs-review-and-post [pr: int] {
          let ssh_key = $"($env.HOME)/.ssh/id_ed25519"
          let builders = $"ssh-ng://aciceri@darwin-build-box.nix-community.org x86_64-darwin,aarch64-darwin ($ssh_key); ssh-ng://root@sisko.wg.aciceri.dev aarch64-linux ($ssh_key)"
          nixpkgs-review pr ($pr) --build-args=$"--builders '($builders)'" --post-result --systems all
         }
      '';
    };
}
