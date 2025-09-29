{
  flake.modules.homeManager.workstation =
    { pkgs, ... }:
    {
      home.packages = with pkgs; [
        # 2d stuff
        gimp
        inkscape

        # 3d stuff
        freecad-wayland
        openscad
        cura-appimage
        blender

        # Office
        libreoffice

        # Chats
        telegram-desktop
        slack

        # Ham radio
        sdrangel
        chirp

        # Other
        calibre
        tremotesf
        vial
        trilium-next-desktop
        claude-desktop
        mcp-proxy # used by claude-desktop to connect to remote MCP servers
        spotube
      ];

      programs = {
        vesktop.enable = true;

        yt-dlp = {
          enable = true;
          package = pkgs.yt-dlp-latest;
        };

        obs-studio = {
          enable = true;
          plugins = with pkgs.obs-studio-plugins; [ wlrobs ];
        };

        element-desktop.enable = true;
      };

      services.remmina.enable = true;
    };
}
