{ lib, ... }:
{
  configurations.nixos.sisko.module =
    { pkgs, ... }:
    {
      systemd.tmpfiles.rules = [
        "d /export 770 nobody nogroup"
        "d /mnt/hd/seedvault 0750 seedvault-sync seedvault-sync -"
      ];

      users.users.seedvault-sync = {
        isSystemUser = true;
        group = "seedvault-sync";
        home = "/mnt/hd/seedvault";
        createHome = false;
      };

      users.groups.seedvault-sync = { };

      fileSystems."/export/hd" = {
        device = "/mnt/hd";
        options = [ "bind" ];
      };

      services.nfs.server = {
        enable = true;
        exports = ''
          /export     10.100.0.1/24(rw,fsid=0,no_subtree_check)
          /export/hd  10.100.0.1/24(rw,nohide,insecure,no_subtree_check,no_root_squash)
        '';
      };

      systemd.services.nfs-server.preStart = ''
        chmod -R 775 /export/hd/torrent
      '';

      services.webdav-server-rs = {
        enable = true;
        settings = {
          server.listen = [
            "0.0.0.0:9999"
            "[::]:9999"
          ];
          location = [
            {
              route = [ "/torrent/*path" ];
              directory = "/mnt/hd/torrent";
              handler = "filesystem";
              methods = [ "webdav-ro" ];
              autoindex = true;
              auth = "false";
            }
          ];
        };
      };

      services.samba-wsdd = {
        enable = true;
        openFirewall = true;
      };

      services.avahi = {
        publish.enable = true;
        publish.userServices = true;
        enable = true;
        openFirewall = true;
      };

      services.samba = {
        enable = true;
        package = pkgs.samba4Full;
        settings = {
          torrent = {
            path = "/mnt/hd/torrent";
            comment = "hd";
            "force user" = "transmission";
            browseable = "yes";
            writeable = "yes";
          };
          seedvault = {
            path = "/mnt/hd/seedvault";
            comment = "seedvault";
            "force user" = "seedvault-sync";
            browseable = "yes";
            writeable = "yes";
          };
        };
      };

      users.users.webdav.extraGroups = [ "transmission" ];

      systemd.services.samba-setup-password = {
        description = "Setup Samba password for transmission user";
        wantedBy = [ "multi-user.target" ];
        after = [ "samba-smbd.service" ];
        serviceConfig = {
          Type = "oneshot";
          RemainAfterExit = true;
        };
        # Can't find a way to make samba accessible from Windows without setting a password
        # Don't care too much about this password since it's only accessible locally or through the VPN
        script = ''
          (echo "transmission"; echo "transmission") | ${lib.getExe' pkgs.samba "smbpasswd"} -a -s transmission
          (echo "seedvault-sync"; echo "seedvault-sync") | ${lib.getExe' pkgs.samba "smbpasswd"} -a -s seedvault-sync
        '';
      };

      # My LAN is trusted
      networking.firewall = {
        allowedTCPPorts = [
          2049
          9999
          139
          445
        ];
        allowedUDPPorts = [
          137
          138
        ];
      };
    };

  flake.modules.nixos.pc = {
    fileSystems."/mnt/sisko" = {
      device = "sisko.wg.aciceri.dev:/export/hd";
      fsType = "nfs";
      options = [
        "x-systemd.automount"
        "noauto"
        "x-systemd.idle-timeout=600"
        "ro"
      ];
    };
  };
}
