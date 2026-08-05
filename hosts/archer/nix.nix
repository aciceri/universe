{
  configurations.darwin.archer.module =
    { config, ... }:
    {
      # Linux remote builder running in a local VM, so we can build Linux
      # derivations from archer (e.g. for sisko/picard/pike).
      nix.linux-builder = {
        enable = true;
        ephemeral = true;
        config = {
          virtualisation.cores = 8;
          virtualisation.darwin-builder.memorySize = 32 * 1024; # 32 GB
          virtualisation.darwin-builder.diskSize = 200 * 1024; # 200 GB
        };
      };

      nix.distributedBuilds = true;
      nix.buildMachines = [
        {
          hostName = "builder.geosurge.ai";
          protocol = "ssh-ng";
          sshUser = "remote";
          sshKey = config.age.secrets.ssh_user_key_ccr.path;
          system = "x86_64-linux";
          maxJobs = 64;
          speedFactor = 2;
          supportedFeatures = [
            "nixos-test"
            "benchmark"
            "big-parallel"
            "kvm"
          ];
        }
      ];
    };
}
