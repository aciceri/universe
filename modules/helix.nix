{
  flake.modules.homeManager.base = {
    programs.helix = {
      enable = true;
      defaultEditor = true; # doesn't seem to work with nushell
      settings = {
        editor = {
          cursor-shape = {
            insert = "bar";
            normal = "block";
            select = "underline";
          };
          color-modes = true;
          true-color = true;
        };
      };
      languages = {
        language = [
          {
            name = "nix";
            language-servers = [ "nixd" ];
            formatter.command = "nixfmt";
          }
          {
            name = "typescript";
            language-servers = [ "vtsls" ];
          }
        ];
        language-server = {
          nixd.command = "nixd";
        };
      };
    };

    programs.nushell.environmentVariables.EDITOR = "hx";
  };
}
