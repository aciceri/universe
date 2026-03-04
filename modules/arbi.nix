{ inputs, ... }:
{
  configurations.nixos.sisko.module =
    { config, ... }:
    {
      imports = [ inputs.arbi.nixosModules.arbi ];
      secrets.arbi_environment_file.owner = config.services.arbi.user;

      services.arbi = {
        enable = true;
        environmentFile = config.age.secrets.arbi_environment_file.path;
        log_level = "debug";
      };

      environment.persistence."/persist".directories = [
        config.services.arbi.dataDir
      ];
    };
}
