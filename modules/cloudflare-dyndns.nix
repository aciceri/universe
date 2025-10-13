{
  configurations.nixos.sisko.module =
    { config, ... }:
    {
      secrets.cloudflare_dyndns_api_token = { };
      services.cloudflare-dyndns = {
        enable = true;
        ipv4 = true;
        ipv6 = false; # not anymore 😭
        domains = [
          "aciceri.dev"
          "blog.aciceri.dev"
          "cv.aciceri.dev"
          "git.aciceri.dev"
          "home.aciceri.dev"
          "photos.aciceri.dev"
          "jelly.aciceri.dev"
          "vpn.aciceri.dev"
          "auth.aciceri.dev"
          "talks.aciceri.dev"
          "*.talks.aciceri.dev"
        ];
        apiTokenFile = config.age.secrets.cloudflare_dyndns_api_token.path;
      };
    };
}
