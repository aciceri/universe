{ config, lib, ... }:
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
          package = pkgs.yt-dlp-master;
        };

        obs-studio = {
          enable = true;
          plugins = with pkgs.obs-studio-plugins; [ wlrobs ];
        };

        element-desktop.enable = true;
      };

      services.remmina.enable = true;
    };

  flake.modules.nixos.workstation =
    { pkgs, ... }:
    {
      virtualisation.libvirtd = {
        enable = true;
        qemu.vhostUserPackages = [ pkgs.virtiofsd ];
      };
      programs.virt-manager.enable = true;
      users.users =
        config.users
        |> lib.mapAttrs (
          _: user: {
            extraGroups = lib.optional user.god "libvirtd";
          }
        );
    };
}
