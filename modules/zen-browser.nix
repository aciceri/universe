{ inputs, ... }:
{
  flake.modules.homeManager.pc =
    {
      config,
      osConfig,
      pkgs,
      lib,
      ...
    }:
    let
      profileCfg = config.programs.zen-browser.profiles.default;

      mkPluginUrl = id: "https://addons.mozilla.org/firefox/downloads/latest/${id}/latest.xpi";

      mkExtensionEntry =
        {
          id,
          pinned ? false,
        }:
        let
          base = {
            install_url = mkPluginUrl id;
            installation_mode = "force_installed";
          };
        in
        if pinned then base // { default_area = "navbar"; } else base;

      mkExtensionSettings = lib.mapAttrs (_: entry: if lib.isAttrs entry then entry else mkExtensionEntry { id = entry; });

      getFirefoxExtensionId = pkgs.writers.writeNuBin "get-firefox-extension-id" { } ''
        def main [url: string] {
          let manifest = http get $url | ^${pkgs.libarchive}/bin/bsdtar -xOf - manifest.json | from json
          let id = $manifest | get applications?.gecko?.id? | default ($manifest | get browser_specific_settings?.gecko?.id?)

          echo $id
        }
      '';

      zenTabSwitcher = pkgs.writers.writeNu "zen-tab-switcher.nu" { } ''
        def parse-tabs [] {
          ^${lib.getExe' pkgs.brotab "brotab"} list
          | from tsv --noheaders
          | rename id title url
        }

        def main [...args] {
          if ($args | is-empty) {
            let tabs = (parse-tabs)
            ["New window"] ++ ($tabs | get title) | str join "\n"
          } else {
            let selection = ($args | str join " ")

            if $selection == "New window" {
              ^sh -c "nohup zen-beta --new-window >/dev/null 2>&1 &"
            } else {
              let tabs = (parse-tabs)
              let matched = ($tabs | where title == $selection | first)

              ^${lib.getExe' pkgs.brotab "brotab"} activate $matched.id

              let zen_windows = (
                ^niri msg -j windows | from json
                | where app_id == "zen-beta"
                | where { |w| $w.title | str contains $matched.title }
              )

              if not ($zen_windows | is-empty) {
                ^niri msg action focus-window --id ($zen_windows | first | get id)
              }
            }
          }
        }
      '';

      rofiZenTabs = pkgs.writeShellScriptBin "rofi-zen-tabs" ''
        exec ${lib.getExe config.programs.rofi.package} -show brotab -modi "brotab:${zenTabSwitcher}"
      '';
    in
    {
      imports = [ inputs.zen-browser.homeModules.beta ];

      home.packages = [
        getFirefoxExtensionId
        rofiZenTabs
        pkgs.brotab
      ];

      xdg.mimeApps.defaultApplications = {
        "application/pdf" = [ "zen-beta.desktop" ];

        "text/html" = [ "zen-beta.desktop" ];
        "application/xhtml+xml" = [ "zen-beta.desktop" ];

        "x-scheme-handler/http" = [ "zen-beta.desktop" ];
        "x-scheme-handler/https" = [ "zen-beta.desktop" ];

        "x-scheme-handler/ftp" = [ "zen-beta.desktop" ];
        "application/x-extension-htm" = [ "zen-beta.desktop" ];
        "application/x-extension-html" = [ "zen-beta.desktop" ];
        "application/x-extension-shtml" = [ "zen-beta.desktop" ];
        "application/x-extension-xhtml" = [ "zen-beta.desktop" ];
        "application/x-extension-xht" = [ "zen-beta.desktop" ];
      };

      stylix.targets.zen-browser.profileNames = [ "default" ];

      # Using `nativeMesagingHosts = [ brotab ]` doesn't seem to work
      home.file.".zen/native-messaging-hosts/brotab_mediator.json".text = builtins.toJSON {
        name = "brotab_mediator";
        description = "This mediator exposes interface over TCP to control browser's tabs";
        path = lib.getExe' pkgs.brotab "bt_mediator";
        type = "stdio";
        allowed_extensions = [ "brotab_mediator@example.org" ];
      };

      programs.niri.settings.binds."Mod+b".action = config.lib.niri.actions.spawn <| lib.getExe rofiZenTabs;

      programs.zen-browser = {
        enable = true;

        policies = {
          AutofillAddressEnabled = true;
          AutofillCreditCardEnabled = false;
          DisableAppUpdate = true;
          DisableFeedbackCommands = true;
          DisableFirefoxStudies = true;
          DisablePocket = false;
          DisableTelemetry = true;
          DontCheckDefaultBrowser = true;
          OfferToSaveLogins = true;
          PasswordManagerEnabled = false;
          DefaultDownloadDirectory = "${config.home.homeDirectory}/Downloads";
          EnableTrackingProtection = {
            Value = true;
            Locked = true;
            Cryptomining = true;
            Fingerprinting = true;
          };
          ExtensionSettings = mkExtensionSettings {
            # As id use $ID as in https://addons.mozilla.org/en-US/firefox/addon/$ID
            "uBlock0@raymondhill.net" = mkExtensionEntry {
              id = "ublock-origin";
              pinned = true;
            };
            "{7a7a4a92-a2a0-41d1-9fd7-1e92480d612d}" = "styl-us";
            "brotab_mediator@example.org" = "brotab";
            "webextension@metamask.io" = "ether-metamask";
            "amptra@keepa.com" = "keepa";
            "{446900e4-71c2-419f-a6a7-df9c091e268b}" = mkExtensionEntry {
              id = "bitwarden-password-manager";
              pinned = true;
            };
          };
        };

        profiles.default = {
          id = 0;
          isDefault = true;

          userChrome = lib.mkAfter ''
            /* Remove gap at the top of the window */
            #navigator-toolbox {
              padding-top: 0 !important;
              margin-top: 0 !important;
            }

            #titlebar {
              margin-top: 0 !important;
              padding-top: 0 !important;
            }

            :root {
              --zen-element-separation: 0px !important;
            }

            /* Disable all corner radius - let niri handle it via clip-to-geometry
               FIXME: this disables evertyhing, also internal elements */
            * {
              border-radius: 0 !important;
            }
          '';

          settings = {
            "zen.tabs.show-newtab-vertical" = false;
            "zen.urlbar.behavior" = "float";
            "zen.view.compact.enable-at-startup" = true;
            "zen.view.compact.hide-toolbar" = true;
            "zen.view.compact.toolbar-flash-popup" = true;
            "zen.view.show-newtab-button-top" = false;
            "zen.view.window.scheme" = 0;
            "zen.welcome-screen.seen" = true;
            "zen.workspaces.continue-where-left-off" = true;

            "identity.fxaccounts.enabled" = true;
            "services.sync.engine.addons" = false;
            "services.sync.engine.bookmarks" = true;
            "services.sync.engine.history" = true;
            "services.sync.engine.passwords" = false;
            "services.sync.engine.prefs" = true;
            "services.sync.engine.tabs" = true;
            "services.sync.engine.creditcards" = false;
            "services.sync.engine.addresses" = true;
            "services.sync.client.name" = "${config.home.username}-${osConfig.networking.hostName}";
          };

          bookmarks = {
            force = true;
            settings = [ ];
          };

          containersForce = true;
          containers = {
            Personal = {
              color = "green";
              icon = "dollar";
              id = 1;
            };
            MLabs = {
              color = "blue";
              icon = "dollar";
              id = 2;
            };
            Proda = {
              color = "yellow";
              icon = "dollar";
              id = 3;
            };
          };

          spacesForce = true;
          spaces = {
            "Personal" = {
              container = profileCfg.containers."Personal".id;
              id = "572910e1-4468-4832-a869-0b3a93e2f165";
              position = 1000;
            };
            "MLabs" = {
              id = "ec287d7f-d910-4860-b400-513f269dee77";
              container = profileCfg.containers."MLabs".id;
              position = 1001;
            };
            "Proda" = {
              container = profileCfg.containers."Proda".id;
              id = "2441acc9-79b1-4afb-b582-ee88ce554ec0";
              position = 1002;
            };
          };

          search = {
            force = true;
            default = "google";
            engines =
              let
                nixSnowflakeIcon = "${pkgs.nixos-icons}/share/icons/hicolor/scalable/apps/nix-snowflake.svg";
              in
              {
                "Nix Packages" = {
                  urls = [
                    {
                      template = "https://search.nixos.org/packages";
                      params = [
                        {
                          name = "type";
                          value = "packages";
                        }
                        {
                          name = "channel";
                          value = "unstable";
                        }
                        {
                          name = "query";
                          value = "{searchTerms}";
                        }
                      ];
                    }
                  ];
                  icon = nixSnowflakeIcon;
                  definedAliases = [ "np" ];
                };
                "NixOS Options" = {
                  urls = [
                    {
                      template = "https://search.nixos.org/options";
                      params = [
                        {
                          name = "channel";
                          value = "unstable";
                        }
                        {
                          name = "query";
                          value = "{searchTerms}";
                        }
                      ];
                    }
                  ];
                  icon = nixSnowflakeIcon;
                  definedAliases = [ "nop" ];
                };
                "Home Manager Options" = {
                  urls = [
                    {
                      template = "https://home-manager-options.extranix.com/";
                      params = [
                        {
                          name = "query";
                          value = "{searchTerms}";
                        }
                        {
                          name = "release";
                          value = "master"; # unstable
                        }
                      ];
                    }
                  ];
                  icon = nixSnowflakeIcon;
                  definedAliases = [ "hmop" ];
                };
                "Noogle" = {
                  urls = [
                    {
                      template = "https://noogle.dev/q";
                      params = [
                        {
                          name = "term";
                          value = "{searchTerms}";
                        }
                      ];
                    }
                  ];
                  icon = nixSnowflakeIcon;
                  definedAliases = [ "noogle" ];
                };
                "GitHub" = {
                  urls = [
                    {
                      template = "https://github.com/search";
                      params = [
                        {
                          name = "q";
                          value = "{searchTerms}";
                        }
                      ];
                    }
                  ];
                  definedAliases = [
                    "gh"
                    "git"
                    "github"
                  ];
                };
                "Hoogle" = {
                  urls = [
                    {
                      template = "https://hoogle.haskell.org/";
                      params = [
                        {
                          name = "hoogle";
                          value = "{searchTerms}";
                        }
                      ];
                    }
                  ];
                  definedAliases = [ "hoogle" ];
                };
                "Stack Overflow" = {
                  urls = [
                    {
                      template = "https://stackoverflow.com/search";
                      params = [
                        {
                          name = "q";
                          value = "{searchTerms}";
                        }
                      ];
                    }
                  ];
                  definedAliases = [ "so" ];
                };
                "Arch Wiki" = {
                  urls = [
                    {
                      template = "https://wiki.archlinux.org/index.php";
                      params = [
                        {
                          name = "search";
                          value = "{searchTerms}";
                        }
                      ];
                    }
                  ];
                  definedAliases = [ "aw" ];
                };
                "youtube" = {
                  urls = [
                    {
                      template = "https://www.youtube.com/results";
                      params = [
                        {
                          name = "search_query";
                          value = "{searchTerms}";
                        }
                      ];
                    }
                  ];
                  definedAliases = [
                    "yt"
                    "youtube"
                  ];
                };
              };
          };
        };
      };
    };
}
