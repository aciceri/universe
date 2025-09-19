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
        (pkgs.discord.override {
          withOpenASAR = true;
          withVencord = true;
        })
        fluffychat
        telegram-desktop
        slack

        # Ham radio
        sdrangel
        chirp

        # Other
        calibre
        remmina
        obs-studio
        tremotesf
        vial
        trilium-next-desktop
        claude-desktop
        spotube
        yt-dlp # needed by spotube
      ];
    };
}
