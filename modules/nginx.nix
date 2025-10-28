{
  configurations.nixos.sisko.module =
    { config, ... }:
    {
      secrets.cloudflare_api_tokens = { };

      security.acme = {
        acceptTerms = true;
        defaults.email = "andrea.ciceri@autistici.org";
        certs = {
          "aciceri.dev" = {
            reloadServices = [ "nginx.service" ];
            domain = "aciceri.dev";
            extraDomainNames = [
              "*.sisko.zt.aciceri.dev"
              "*.sisko.wg.aciceri.dev"
            ];
            dnsProvider = "cloudflare";
            dnsPropagationCheck = true;
            group = config.services.nginx.group;
            environmentFile = config.age.secrets.cloudflare_api_tokens.path;
          };
        };
      };

      networking.firewall.allowedTCPPorts = [
        80
        443
      ];

      services.nginx = {
        enable = true;
        statusPage = true;
        recommendedGzipSettings = true;
        recommendedOptimisation = true;
        recommendedProxySettings = true;
        recommendedTlsSettings = true;

        virtualHosts."aciceri.dev" = {
          forceSSL = true;
          useACMEHost = "aciceri.dev";
          extraConfig = ''
            return 301 https://blog.aciceri.dev$request_uri;
          '';
        };
      };

      environment.persistence."/persist".directories = [
        "/var/lib/acme"
      ];
    };
}
