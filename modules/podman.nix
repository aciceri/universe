{
  flake.modules.nixos.base =
    { pkgs, ... }:
    {
      boot.binfmt.preferStaticEmulators = true;

      virtualisation.podman = {
        enable = true;
        dockerCompat = true;
        defaultNetwork.settings.dns_enabled = true;
      };

      environment.systemPackages = with pkgs; [
        podman-compose
      ];
    };
}
