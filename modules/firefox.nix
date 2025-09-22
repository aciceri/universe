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
}
