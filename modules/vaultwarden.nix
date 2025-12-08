{
  configurations.nixos.sisko.module =
    { config, ... }:
    let
      domain = "vault.sisko.wg.aciceri.dev";
      cfg = config.services.vaultwarden;
    in
    {
      secrets.vaultwarden_admin_token.owner = "vaultwarden";

      systemd.tmpfiles.rules = [
        "d ${cfg.backupDir} 770 vaultwarden vaultwarden"
      ];

      environment.persistence."/persist".directories = [
        "/var/lib/bitwarden_rs"
        cfg.backupDir
      ];

      services.vaultwarden = {
        enable = true;
        dbBackend = "sqlite"; # Postgres is supported but using sqlite we can leverage systemd hardening
        backupDir = "/var/backup/vaultwarden";
        config = {
          DOMAIN = "https://${domain}";
          ROCKET_ADDRESS = "127.0.0.1";
          ROCKET_PORT = 8222;

          SIGNUPS_ALLOWED = false;
          INVITATIONS_ALLOWED = true;
          WEBSOCKET_ENABLED = true;

          SMTP_HOST = "smtp.autistici.org";
          SMTP_FROM = "andrea.ciceri@autistici.org";
          SMTP_PORT = 587;
          SMTP_SECURITY = "starttls";
          SMTP_USERNAME = "andrea.ciceri@autistici.org";
        };
        environmentFile = config.age.secrets.vaultwarden_admin_token.path;
      };

      services.nginx.virtualHosts."${domain}" = {
        forceSSL = true;
        useACMEHost = "aciceri.dev";
        locations."/" = {
          proxyPass = "http://localhost:${builtins.toString cfg.config.ROCKET_PORT}";
          proxyWebsockets = true;
        };
        serverAliases = [ "vault.sisko.zt.aciceri.dev" ];
        extraConfig = ''
          allow 10.100.0.0/24;
          allow 10.100.1.0/24;
          allow 127.0.0.1;
          deny all;
        '';
      };
    };
}
