{ inputs, ... }:
{
  flake.modules.homeManager.pc =
    { config, pkgs, ... }:
    let
      profileCfg = config.programs.zen-browser.profiles.default;
    in
    {
      imports = [ inputs.zen-browser.homeModules.beta ];

      stylix.targets.zen-browser.profileNames = [ "default" ];

      programs.zen-browser = {
        enable = true;
        profiles.default = {
          id = 0;
          isDefault = true;
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
          };

          bookmarks = {
            force = true;
            settings = [
              {
                name = "Nix sites";
                toolbar = true;
                bookmarks = [
                  {
                    name = "homepage";
                    url = "https://nixos.org/";
                  }
                  {
                    name = "wiki";
                    tags = [
                      "wiki"
                      "nix"
                    ];
                    url = "https://wiki.nixos.org/";
                  }
                ];
              }
            ];
          };

          pinsForce = true;
          pins = { };

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
              icon = "🎭";
              position = 1000;
              theme = {
                type = "gradient";
                colors = [
                  {
                    # red = 216;
                    # green = 204;
                    # blue = 235;
                    algorithm = "floating";
                    type = "explicit-lightness";
                  }
                ];
                opacity = 0.8;
                texture = 0.5;
              };
            };
            "MLabs" = {
              id = "ec287d7f-d910-4860-b400-513f269dee77";
              container = profileCfg.containers."MLabs".id;
              icon = "💌";
              position = 1001;
              theme = {
                type = "gradient";
                colors = [
                  {
                    # red = 171;
                    # green = 219;
                    # blue = 227;
                    algorithm = "floating";
                    type = "explicit-lightness";
                  }
                ];
                opacity = 0.2;
                texture = 0.5;
              };
            };
            "Proda" = {
              container = profileCfg.containers."Proda".id;
              id = "2441acc9-79b1-4afb-b582-ee88ce554ec0";
              icon = "💸";
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
              };
          };
        };
      };
    };
}
