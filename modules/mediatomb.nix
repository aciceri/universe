{
  configurations.nixos.sisko.module =
    { pkgs, ... }:
    {
      services.mediatomb = {
        enable = true;
        openFirewall = true;
        serverName = "Sisko";
        interface = "lan0";
        mediaDirectories = [
          {
            path = "/tank/media/movies";
            recursive = true;
          }
          {
            path = "/tank/media/series";
            recursive = true;
          }
        ];

        # gerbera 3.0.0 predates two breaking changes in its dependencies, so
        # nixpkgs cannot build it as packaged. Drop all of this once nixpkgs
        # ships a gerbera release that carries both fixes.
        #
        # 1. It calls std::strerror, std::memcpy and std::strchr without
        #    including <cstring>, which GCC 15 rejects. Fixed upstream after
        #    the release.
        # 2. fmt 12 moved `fmt::format` out of the header gerbera includes, so
        #    every fmt::format call fails to resolve. Build it against fmt 11
        #    instead — and spdlog too, since gerbera links both and they must
        #    agree on the fmt they were compiled against.
        package =
          (pkgs.gerbera.override {
            fmt = pkgs.fmt_11;
            spdlog = pkgs.spdlog.override { fmt = pkgs.fmt_11; };
          }).overrideAttrs
            (old: {
              patches = (old.patches or [ ]) ++ [
                (pkgs.fetchpatch {
                  name = "gerbera-include-cstring.patch";
                  url = "https://github.com/gerbera/gerbera/commit/7caa9ae9ebf5c4cded5a292701528ff1d5c4946e.patch";
                  hash = "sha256-ZZpwiXKOV2RfzmBLl7iTeJ8y8gMJ0QgodMfalJIO4E4=";
                  # 3.0.0 does not include <pugixml.hpp> in this file, which the
                  # upstream hunk uses as context. It needs the include all the
                  # same — it calls std::strerror — so it is added below instead.
                  excludes = [ "src/config/setup/config_setup_path.cc" ];
                })
              ];
              postPatch = (old.postPatch or "") + ''
                substituteInPlace src/config/setup/config_setup_path.cc \
                  --replace-fail '#include "util/logger.h"' \
                    '#include "util/logger.h"${"\n"}${"\n"}#include <cstring>'
              '';
            });
      };

      users.users.mediatomb.extraGroups = [
        "radarr"
        "sonarr"
      ];
    };
}
