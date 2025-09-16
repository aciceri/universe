{
  configurations.nixos.sisko.module =
    let
      dir = "/mnt/hd/ncps";
    in
    {
      systemd.tmpfiles.rules = [
        "d ${dir} 770 ncps ncps"
      ];

      services.ncps = {
        enable = true;
        cache = {
          hostName = "ncps.sisko.wg.aciceri.dev";
          dataPath = "${dir}/data";
          tempPath = "${dir}/tmp";
          databaseURL = "sqlite:${dir}/db/db.sqlite";
          maxSize = "200G";
          lru.schedule = "0 2 * * *"; # Clean up daily at 2 AM
          allowPutVerb = true;
          allowDeleteVerb = true;
        };
        server.addr = "0.0.0.0:8501";
        upstream = {
          caches = [
            "https://cache.nixos.org"
            "https://nix-community.cachix.org"
            "https://mlabs.cachix.org"
            "http://sisko.wg.aciceri.dev:8081/sisko"
          ];
          publicKeys = [
            "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
            "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
            "mlabs.cachix.org-1:gStKdEqNKcrlSQw5iMW6wFCj3+b+1ASpBVY2SYuNV2M="
            "sisko:4A3G4hgZVjhfPLh7Hy9V6xhRzRJp1l4fDDbLqQrQsbU="
          ];
        };
        prometheus.enable = true;
      };
    };
}
