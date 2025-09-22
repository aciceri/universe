{ lib, ... }:
{
  flake.modules.homeManager.pc = lib.mkMerge [
    (
      { pkgs, ... }:
      {
        programs.thunderbird = {
          # We force thunderbird to run as X11 application otherwise birdtray cannot detect if the window is open
          # https://github.com/gyunaev/birdtray/pull/584
          package = pkgs.stdenv.mkDerivation {
            pname = "thunderbird-x11";
            inherit (pkgs.thunderbird) version meta;
            src = pkgs.thunderbird;
            nativeBuildInputs = [ pkgs.makeWrapper ];
            installPhase = ''
              mkdir -p $out/bin
              ln -s ${pkgs.thunderbird}/bin/thunderbird $out/bin/thunderbird
              wrapProgram $out/bin/thunderbird \
                --set GDK_BACKEND x11
            '';
          };
          settings = {
            "messenger.startup.action" = 0; # Don't open chat on startup (0=no action)
            "purple.conversations.im.send_typing" = false; # Don't send "typing" notification

            "rss.display.prefer_plaintext" = true; # Prefer plaintext for RSS feeds
            "rss.show.summary" = false; # Don't show RSS feed summaries

            "ldap_2.servers.history.maxHits" = 0; # Don't save LDAP search history

            "mail.tabs.drawInTitlebar" = true; # Draw tabs in title bar
            "toolkit.legacyUserProfileCustomizations.stylesheets" = true; # Enable custom CSS (userChrome.css)
            "mailnews.start_page.enabled" = false; # Disable welcome/start page
            "mail.tabs.autoHide" = true; # Hide tab bar when only one tab is open
            "mail.pane_config.dynamic" = 0; # Classic 3-pane layout (0=classic, 1=wide, 2=vertical)

            "privacy.donottrackheader.enabled" = true; # Send "Do Not Track" header
            "mail.spam.manualMark" = true; # Mark spam manually (not automatically)

            "toolkit.telemetry.enabled" = false; # Disable Mozilla telemetry
            "toolkit.crashreporter.enabled" = false; # Disable automatic crash reports
            "datareporting.healthreport.uploadEnabled" = false; # Disable health report data upload
            "datareporting.policy.dataSubmissionEnabled" = false; # Disable data submission in general

            "app.update.enabled" = false; # Disable automatic app updates
            "extensions.update.enabled" = false; # Disable automatic extension updates

            "mail.compose.default_to_paragraph" = true; # Use paragraphs instead of <br> in HTML
            "mailnews.send_plaintext_flowed" = true; # Send plaintext with format=flowed (better compatibility)

            "mail.openpgp.enable" = true; # Enable OpenPGP support for email encryption

            "mail.showCondensedAddresses" = false; # Show full email addresses (not condensed)
            "mailnews.mark_message_read.auto" = false; # Don't automatically mark messages

            "mail.minimizeToTray" = true; # Enable minimize to tray
            "mail.close_message_window.on_delete" = true; # Close window when deleting message
            "extensions.unifiedtoolbar.enabled" = false; # Disable unified toolbar for better tray support

            "browser.tabs.closeWindowWithLastTab" = false; # Don't close app when closing last tab
            "mail.biff.show_tray_icon" = true; # Show icon in system tray
            "mail.biff.show_tray_icon_always" = true; # Always show tray icon (even when no new mail)
          };
        };
      }
    )
    (
      { config, ... }:
      lib.mkIf (config.home.username == "ccr") {
        programs.thunderbird.enable = true;
        programs.thunderbird.profiles.ccr.isDefault = true;

        accounts.email = {
          accounts.autistici = {
            aerc.enable = true;
            address = "andrea.ciceri@autistici.org";
            gpg = {
              key = "9320F317C9DE72DA75A1391D7101E4EA6160968D";
              signByDefault = true;
            };
            imap = {
              host = "mail.autistici.org";
              port = 993;
            };
            primary = true;
            realName = "Andrea Ciceri";
            signature = {
              text = ''
                Andrea Ciceri
              '';
              showSignature = "append";
            };
            smtp = {
              host = "smtp.autistici.org";
            };
            userName = "andrea.ciceri@autistici.org";
            thunderbird.enable = true;
          };
          accounts.mlabs = {
            address = "andreaciceri@mlabs.city";
            imap = {
              host = "imap.gmail.com";
              port = 993;
            };
            realName = "Andrea Ciceri";
            smtp.host = "smtp.gmail.com";
            userName = "andreaciceri@mlabs.city";
            thunderbird.enable = true;
          };
        };
      }
    )
  ];
}
