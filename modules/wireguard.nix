{ lib, ... }:
{
  flake.modules.nixos.base =
    { config, pkgs, ... }:
    let
      cfg = config.wireguard;
      inherit (config.networking) hostName;
      hostSubmoduleType = lib.types.submodule (
        { name, ... }:
        {
          options = {
            name = lib.mkOption {
              type = lib.types.str;
              default = name;
            };
            ip = lib.mkOption {
              type = lib.types.str;
            };
            publicKey = lib.mkOption {
              type = lib.types.str;
            };
          };
        }
      );
    in
    {
      options.wireguard = {
        isClient = lib.mkOption {
          type = lib.types.bool;
          default = hostName != "sisko";
        };
        interfaceName = lib.mkOption {
          type = lib.types.str;
          default = "wg0";
        };
        publicKey = lib.mkOption {
          type = lib.types.str;
          default = "wg0";
        };
        hosts = lib.mkOption {
          type = lib.types.attrsOf hostSubmoduleType;
          default = { };
        };
      };
      config = lib.mkMerge [
        {
          wireguard.hosts = {
            sisko = {
              ip = "10.100.0.1";
              publicKey = "bc5giljukT1+ChbbyTLdOfejfR3c8RZ4XoXmQM54nTY=";
            };
            picard = {
              ip = "10.100.0.2";
              publicKey = "O9V2PI7+vZm7gGn3f9SaTsJbVe9urf/jZkdXFz/mjVU=";
            };
            oneplus8t = {
              ip = "10.100.0.4";
              publicKey = "3odi1jFpq+9Ax43bBlPqQXq3Gc90c1o5ewP//lE2FCE=";
            };
            kirk = {
              ip = "10.100.0.3";
              publicKey = "GrCpICbC25FQ+7JXgOJ9btvZp8YI/uecyBXx9IevsBo=";
            };
            pike = {
              ip = "10.100.0.8";
              publicKey = "16ctjunXCXDPLSUhocstJ9z9l45/YuJFxlLkpoxChjI=";
            };
          };

          secrets."wireguard_private_key_${hostName}" = { };
          networking.firewall.trustedInterfaces = [ cfg.interfaceName ];
          networking.wireguard.interfaces.${cfg.interfaceName} = {
            mtu = 1200;
            privateKeyFile = config.age.secrets."wireguard_private_key_${hostName}".path;
            listenPort = 51820;
          };
        }
        (lib.mkIf cfg.isClient {
          networking.wireguard.interfaces.wg0 = {
            ips = [ "${cfg.hosts.${hostName}.ip}/32" ];
            peers = [
              {
                publicKey = cfg.hosts.sisko.publicKey;
                allowedIPs = [ "10.100.0.0/24" ];
                # allowedIPs = [ "0.0.0.0/24" ]; # Uncomment for full tunnel
                endpoint = "vpn.aciceri.dev:51820";
                persistentKeepalive = 25;
              }
            ];
          };
        })
        (lib.mkIf (!cfg.isClient) {
          networking.nat.enable = true;

          networking.firewall.allowedUDPPorts = [ config.networking.wireguard.interfaces.wg0.listenPort ];

          networking.wireguard.interfaces.wg0 = {
            ips = [ "${cfg.hosts.${hostName}.ip}/24" ];
            peers = lib.mapAttrsToList (_: host: {
              publicKey = host.publicKey;
              allowedIPs = [ "${host.ip}/32" ];
            }) cfg.hosts;

            postSetup = ''
              ${lib.getExe' pkgs.iptables "iptables"} -t nat -A POSTROUTING -s 10.100.0.0/24 -o enP4p65s0 -j MASQUERADE
            '';

            postShutdown = ''
              ${lib.getExe' pkgs.iptables "iptables"} -t nat -D POSTROUTING -s 10.100.0.0/24 -o enP4p65s0 -j MASQUERADE
            '';
          };
        })
      ];
    };
}
