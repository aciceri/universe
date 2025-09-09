{
  flake.modules.homeManager.base = {
    programs.git = {
      enable = true;
    };

    programs.lazygit.enable = true;
  };
}
