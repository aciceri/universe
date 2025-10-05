{ lib, ... }:
{
  configurations.nixos = lib.mkMerge [
    {
      sisko.module =
        { config, pkgs, ... }:
        let
          theme = pkgs.fetchzip {
            url = "https://github.com/catppuccin/gitea/releases/download/v1.0.1/catppuccin-gitea.tar.gz";
            hash = "sha256-et5luA3SI7iOcEIQ3CVIu0+eiLs8C/8mOitYlWQa/uI=";
            stripRoot = false;
          };
          cfg = config.services.forgejo;
          inherit (cfg) stateDir;
        in
        {
          secrets.autistici_password = {
            mode = "770";
            group = "autistici";
          };

          users.groups.autistici.members = [ cfg.user ]; # Group who has access to the autistici_password secret

          services.forgejo = {
            # TODO migrate to Postgres
            enable = true;
            package = pkgs.forgejo; # by default it's forgejo-lts
            settings = {
              DEFAULT = {
                RUN_MODE = "prod";
                APP_NAME = "git.aciceri.dev";
              };
              service.ENABLE_NOTIFY_MAIL = true;
              session.COOKIE_SECURE = true;
              service.DISABLE_REGISTRATION = true;
              server = {
                HTTP_PORT = 3002;
                ROOT_URL = "https://git.aciceri.dev";
                LFS_MAX_FILE_SIZE = 0; # no limit
              };
              federation.ENABLED = true;
              mailer = {
                ENABLED = true;
                PROTOCOL = "smtp+starttls";
                SMTP_ADDR = "smtp.autistici.org";
                SMTP_PORT = 587;
                FROM = "andrea.ciceri@autistici.org";
                USER = "andrea.ciceri@autistici.org";
              };
              other = {
                SHOW_FOOTER_VERSION = false;
              };
              ui = {
                DEFAULT_THEME = "catppuccin-mocha-blue";
                THEMES = builtins.concatStringsSep "," (
                  [ "auto,forgejo-auto,forgejo-dark,forgejo-light,arc-gree,gitea" ]
                  ++ (map (name: lib.removePrefix "theme-" (lib.removeSuffix ".css" name)) (builtins.attrNames (builtins.readDir theme)))
                );
              };
              "ui.meta" = {
                AUTHOR = "Andrea Ciceri";
                DESCRIPTION = "My personal git forge";
                KEYWORDS = "git,self-hosted,forgejo,open-source,nix,nixos";
              };
            };
            lfs = {
              enable = true;
              contentDir = "/mnt/hd/forgejo-lfs";
            };
            secrets.mailer.PASSWD = config.age.secrets.autistici_password.path;
            dump = {
              enable = true;
              backupDir = "/mnt/hd/forgejo-dumps";
            };
          };

          environment.persistence."/persist".directories = [
            config.services.forgejo.stateDir
          ];

          # Add the theme
          systemd.services.forgejo.preStart = lib.mkAfter ''
            rm -rf ${stateDir}/custom/public/assets
            mkdir -p ${stateDir}/custom/public/assets
            ln -sf ${theme} ${stateDir}/custom/public/assets/css
          '';

          services.nginx.virtualHosts."git.aciceri.dev" = {
            enableACME = true;
            forceSSL = true;
            locations."/".proxyPass = "http://127.0.0.1:${builtins.toString cfg.settings.server.HTTP_PORT}";
            extraConfig = ''
              client_max_body_size 100M;
              client_body_timeout 300s;
              client_body_buffer_size 128k;

              rewrite ^/user/login$ /user/oauth2/kanidm;
            '';
          };
        };
    }
    (
      (name: {
        module =
          { config, pkgs, ... }:
          {
            secrets.forgejo_runners_registration_token.owner = "gitea-runner";
            services.gitea-actions-runner =
              let
                numInstances = 8;
                instancesNames = lib.genList (n: "nix-${name}-${builtins.toString n}") numInstances;
              in
              {
                package = pkgs.forgejo-actions-runner;
                instances = lib.genAttrs instancesNames (instanceName: {
                  enable = true;
                  name = instanceName;
                  url = "https://git.aciceri.dev";
                  tokenFile = config.age.secrets.forgejo_runners_registration_token.path;
                  labels = [ "native:host" ];
                  hostPackages = with pkgs; [
                    nodejs
                    busybox
                    attic-client
                    openssh
                    nix
                    git
                    tea
                    jq
                    nixos-rebuild
                  ];
                });
              };
          };
      })
      |> lib.genAttrs [
        "picard"
        "pike"
      ]
    )
  ];

  flake.modules.nixos.base = {
    users.users.root.openssh.authorizedKeys.keys = [
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIJjfUJ/na5sHiniliSdJIJQ5EjUG8UTjqFymuLuVJ2E7" # SSH key used by Forgejo workflows for deployemnts
    ];
  };
}
