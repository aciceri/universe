{ config, lib, ... }:
{
  flake.modules.nixos.workstation = {
    programs.adb.enable = true;

    users.users =
      config.users
      |> lib.mapAttrs (
        _: _: {
          extraGroups = [ "adbusers" ];
        }
      );
  };

  flake.modules.homeManager.workstation =
    { pkgs, ... }:
    let
      patchedSrc = pkgs.stdenv.mkDerivation {
        name = "patched-reinstall-magisk-on-lineageos-source";
        version = "git";
        src = pkgs.fetchFromGitHub {
          owner = "NicolasWebDev";
          repo = "reinstall-magisk-on-lineageos";
          rev = "1ca911ed555d4badd705c6c71750b78be8962b0b";
          hash = "sha256-95LzcWL4efR77i8UlzIT+7wQXp+91K2sUwcjmHvTf+Q=";
        };
        installPhase = ''
          mkdir -p $out/bin
          cp reinstall-magisk-on-lineageos $out/bin/reinstall-magisk-on-lineageos
        '';
        patchPhase = ''
          substituteInPlace reinstall-magisk-on-lineageos \
            --replace-fail "paste_yours_here" "\"\$1\""
        '';
      };
      path = lib.makeBinPath (
        with pkgs;
        [
          android-tools
          jq
        ]
      );
      reinstall-magisk-on-lineageos = pkgs.writeShellScriptBin "reinstall-magisk-on-lineageos" ''
        export PATH=$PATH:${path}
        exec ${patchedSrc}/bin/reinstall-magisk-on-lineageos $1
      '';
    in
    {
      home.packages = [ reinstall-magisk-on-lineageos ];
    };
}
