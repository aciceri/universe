{
  flake.modules.homeManager.base = {
    programs.helix = {
      enable = true;
      defaultEditor = true;
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
  };
}
