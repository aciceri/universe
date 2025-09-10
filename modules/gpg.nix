{
  flake.modules.homeManager.workstation =
    { pkgs, ... }:
    {
      services.gpg-agent = {
        enable = true;
        pinentry.package = pkgs.pinentry-rofi.override {
          rofi = pkgs.rofi-wayland;
        };
        extraConfig = ''
          allow-loopback-pinentry
        '';
      };

      programs.gpg.enable = true;
    };
}
