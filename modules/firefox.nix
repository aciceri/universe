{ lib, ... }:
{
  flake.modules.homeManager.pc =
    { pkgs, config, ... }:
    {
      programs.firefox = {
        enable = true;
        nativeMessagingHosts = with pkgs; [
          tridactyl-native
          vdhcoapp
          firefoxpwa
        ];
        policies = {
          DisableTelemetry = true;
          DisableFirefoxStudies = true;
          EnableTrackingProtection = {
            Value = true;
            Locked = true;
            Cryptomining = true;
            Fingerprinting = true;
            EmailTracking = true;
          };
          HardwareAcceleration = true;
          FirefoxHome = {
            TopSites = false;
            SponsoredTopSites = false;
            Highlights = false;
            Pocket = false;
            SponsoredPocket = false;
            Snippets = false;
            Locked = false;
          };
          FirefoxSuggest = {
            WebSuggestions = true;
            SponsoredSuggestions = false;
            ImproveSuggest = false;
            Locked = true;
          };
          OverrideFirstRunPage = "";
          OverridePostUpdatePage = "";
          DontCheckDefaultBrowser = true;
          DisplayMenuBar = "default-off";
          SearchBar = "unified";
          HttpsOnlyMode = "force_enabled";
          NoDefaultBookmarks = true;
          OfferToSaveLogins = true;
          PasswordManagerEnabled = true;
          DefaultDownloadDirectory = "${config.home.homeDirectory}/Downloads";
          PromptForDownloadLocation = false;
          RequestedLocales = "en-US";
          ExtensionSettings = {
            "uBlock0@raymondhill.net" = {
              installation_mode = "force_installed";
              install_url = "https://addons.mozilla.org/firefox/downloads/latest/ublock-origin/latest.xpi";
            };
            "tridactyl.vim@cmcaine.co.uk" = {
              installation_mode = "force_installed";
              install_url = "https://addons.mozilla.org/firefox/downloads/latest/tridactyl-vim/latest.xpi";
            };
          };
        };
        profiles.default = {
          search.force = true;
          search.default = "google";
          extensions.force = true;
        };
      };
      stylix.targets.firefox = {
        profileNames = [ "default" ];
        colorTheme.enable = true;
      };
    };

  configurations.nixos.sisko.module =
    { config, pkgs, ... }:
    let
      cfg = config.services.firefox-syncserver;
      stateDir = "firefox-syncserver";
    in
    {
      secrets.firefox_sync_secrets.owner = "firefox-syncserver";

      users = {
        groups.firefox-syncserver = { };
        users.firefox-syncserver = {
          group = "firefox-syncserver";
          home = "/var/lib/${stateDir}";
          isSystemUser = true;
        };
      };

      systemd.services.firefox-syncserver.serviceConfig = {
        User = "firefox-syncserver";
        Group = "firefox-syncserver";
        StateDirectory = stateDir;
        DynamicUser = lib.mkForce false;
      };

      environment.persistence."/persist".directories = [
        "/var/lib/${stateDir}"
        config.services.mysql.dataDir
        config.services.mysqlBackup.location
      ];

      services = {
        mysql.package = pkgs.mariadb;
        mysqlBackup = {
          enable = true;
          databases = [ cfg.database.name ];
        };

        firefox-syncserver = {
          enable = true;
          database = {
            createLocally = true;
            name = "firefox_syncserver";
            user = "firefox-syncserver";
            host = "localhost";
          };
          settings = {
            syncstorage = {
              database_url = "mysql://firefox-syncserver@localhost/firefox_syncserver?socket=/run/mysqld/mysqld.sock";
            };
            tokenserver = {
              database_url = "mysql://firefox-syncserver@localhost/firefox_syncserver?socket=/run/mysqld/mysqld.sock";
            };
          };
          secrets = config.age.secrets.firefox_sync_secrets.path;

          singleNode = {
            enable = true;
            enableNginx = true;
            hostname = "firefox-sync.sisko.wg.aciceri.dev";
            url = "https://${cfg.singleNode.hostname}";
            capacity = 1;
          };
        };

        nginx.virtualHosts.${cfg.singleNode.hostname} = {
          forceSSL = lib.mkForce true;
          useACMEHost = "aciceri.dev";
          serverAliases = [ "firefox-sync.sisko.zt.aciceri.dev" ];
        };
      };
    };
}
