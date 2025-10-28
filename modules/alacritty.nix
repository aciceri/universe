{
  flake.modules.homeManager.pc = {
    programs.alacritty = {
      enable = true;
      settings = {
        mouse.hide_when_typing = true;
      };
    };
  };
}
