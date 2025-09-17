{
  flake.modules.homeManager.workstation =
    { pkgs, ... }:
    {
      services.gpg-agent = {
        enable = true;
        pinentry.package = pkgs.pinentry-rofi;
        extraConfig = ''
          allow-loopback-pinentry
        '';
      };

      programs.gpg.enable = true;
    };
}
