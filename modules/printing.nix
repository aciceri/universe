{ config, lib, ... }:
let
  printerIp = "10.1.1.39";
  domain = "cups.sisko.wg.aciceri.dev";
  cupsPort = 631;
in
{
  configurations.nixos.sisko.module = {
    services.avahi = {
      enable = true;
      nssmdns4 = true;
      openFirewall = true;
      publish = {
        enable = true;
        userServices = true;
      };
    };
    hardware.sane.enable = true;

    services.printing = {
      enable = true;
      listenAddresses = [ "*:${toString cupsPort}" ];
      allowFrom = [ "all" ];
      browsing = true;
      defaultShared = true;
      extraConf = ''
        ServerName localhost
        ServerAlias *
        ServerAlias ${domain}
        ServerAlias cups.sisko.zt.aciceri.dev
        WebInterface Yes
        StrictConformance No
        HostNameLookups Off
      '';
    };

    hardware.printers.ensurePrinters = [
      {
        name = "Brother-MFC-L2710DW";
        location = "Home";
        description = "Brother MFC-L2710DW (through Sisko)";
        deviceUri = "ipp://${printerIp}/ipp/print";
        model = "everywhere"; # Use driverless IPP Everywhere
        ppdOptions = {
          PageSize = "A4";
        };
      }
    ];

    services.nginx.virtualHosts.${domain} = {
      forceSSL = true;
      useACMEHost = "aciceri.dev";
      locations."/" = {
        # Don't use `proxyPass` to avoid automatic header includes
        extraConfig = ''
          proxy_pass http://localhost:${toString cupsPort};
          proxy_set_header Host localhost:${toString cupsPort};
        '';
      };
      serverAliases = [ "cups.sisko.zt.aciceri.dev" ];
      extraConfig = ''
        allow 10.100.0.0/24;
        allow 10.100.1.0/24;
        allow 127.0.0.1;
        deny all;
      '';
    };
  };

  flake.modules.nixos.pc = {
    services.avahi = {
      enable = true;
      nssmdns4 = true;
      openFirewall = true;
    };

    services.printing = {
      enable = true;
      browsing = true;
    };

    hardware.sane = {
      enable = true;
      brscan4 = {
        enable = true;
        netDevices = {
          brother = {
            model = "MFC-L2710DW";
            ip = printerIp;
          };
        };
      };
    };

    users.users =
      config.users
      |> lib.mapAttrs (
        _: user: {
          extraGroups = [
            "lp"
            "scanner"
          ];
        }
      );
  };
}
