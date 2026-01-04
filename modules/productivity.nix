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
        simple-scan

        # Chats
        telegram-desktop
        signal-desktop
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
        # winboat # FIXME re-enable after https://github.com/NixOS/nixpkgs/issues/462513
        nur.repos.nltch.spotify-adblock
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

        mpv.enable = true;

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

      hardware = {
        rtl-sdr.enable = true;
        hackrf.enable = true;
      };

      users.users =
        config.users
        |> lib.mapAttrs (
          _: user: {
            extraGroups = lib.optional user.god "libvirtd";
          }
        );
    };
}
