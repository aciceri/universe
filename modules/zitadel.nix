{
  configurations.nixos.sisko.module =
    { config, ... }:
    let
      cfg = config.services.zitadel;
    in
    {
      secrets.zitadel_master_key.owner = config.services.zitadel.user;
      secrets.autistici_password = {
        mode = "770";
        group = "autistici";
      };

      users.groups.autistici.members = [ cfg.user ];

      services.postgresql = {
        ensureDatabases = [ cfg.user ];
        ensureUsers = [
          {
            name = cfg.user;
            ensureDBOwnership = true;
            ensureClauses.login = true;
            ensureClauses.superuser = true;
          }
        ];
      };

      services.zitadel = {
        enable = true;
        tlsMode = "external";
        masterKeyFile = config.age.secrets.zitadel_master_key.path;
        steps = {
          FirstInstance = {
            InstanceName = "aciceri";
            Org = {
              Name = "aciceri";
              Human = {
                UserName = "andrea.ciceri@autistici.org";
                FirstName = "Andrea";
                LastName = "Ciceri";
                Email.Verified = true;
                Password = "Password123$"; # only initial, changed during the first login
                PasswordChangeRequired = true;
              };
            };
            LoginPolicy.AllowRegister = false;
          };
        };
        settings = {
          Port = 2048;
          ExternalDomain = "auth.aciceri.dev";
          Email = {
            SMTP = {
              Host = "smtp.autistici.org";
              Port = 587;
              User = "andrea.ciceri@autistici.org";
              Password._secret = config.age.secrets.autistici_password.path;
              TLS = true;
            };
            From = "andrea.ciceri@autistici.org";
          };
          Database.postgres = {
            Host = "/var/run/postgresql/";
            Port = 5432;
            Database = cfg.user;
            User = {
              Username = cfg.user;
              SSL.Mode = "disable";
            };
            Admin = {
              Username = cfg.user;
              SSL.Mode = "disable";
              ExistingDatabase = cfg.user;
            };
          };
        };
      };

      services.nginx.virtualHosts."${cfg.settings.ExternalDomain}" = {
        forceSSL = true;
        enableACME = true;
        locations."/".proxyPass = "http://localhost:${builtins.toString cfg.settings.Port}";
      };
    };
}
