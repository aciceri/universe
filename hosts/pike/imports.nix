{ config, ... }:
let
  inherit (config.flake) modules;
in
{
  configurations.nixos.pike.module =
    { config, lib, ... }:
    {
      imports = with modules.nixos; [ workstation ];
      home-manager.sharedModules = with modules.homeManager; [
        workstation
      ];

      users.users.ccr.linger = lib.mkIf config.home-manager.users.ccr.multiverse.enable true;

      home-manager.users.ccr =
        {
          config,
          lib,
          pkgs,
          ...
        }:
        let
          runtimeConfig = pkgs.writeText "omp-multiverse.yml" ''
            collab:
              autoStart: control
          '';
          start = pkgs.writeShellScript "omp-multiverse" ''
            set -eu
            ${pkgs.coreutils}/bin/install -d -m 0700 "$STATE_DIRECTORY/sessions"
            exec ${lib.getExe pkgs.llm-agents.omp} \
              --mode rpc \
              --config ${runtimeConfig} \
              --cwd ${lib.escapeShellArg config.multiversePath} \
              --session-dir "$STATE_DIRECTORY/sessions" \
              --continue \
              --model anthropic/claude-opus-5
          '';
          nameSession = pkgs.writeShellScript "omp-multiverse-name" ''
            set -eu
            printf '%s\n' '{"id":"multiverse-startup-name","type":"set_session_name","name":"multiverse"}' > "$1"
          '';
        in
        lib.mkIf config.multiverse.enable {
          systemd.user.sockets.omp-multiverse = {
            Unit = {
              Description = "Private RPC input for the persistent multiverse OMP session";
              # Stopping the service must also stop socket activation.
              PartOf = [ "omp-multiverse.service" ];
            };
            Socket = {
              ListenFIFO = "%t/omp-multiverse/stdin";
              SocketMode = "0600";
              DirectoryMode = "0700";
              RemoveOnStop = true;
              Service = "omp-multiverse.service";
            };
          };

          systemd.user.services.omp-multiverse = {
            Unit = {
              Description = "Persistent headless multiverse OMP session";
              Requires = [ "omp-multiverse.socket" ];
              Wants = [ "omp-session-gateway.service" ];
              After = [
                "omp-multiverse.socket"
                "omp-session-gateway.service"
              ];
              StartLimitIntervalSec = 0;
            };
            Service = {
              WorkingDirectory = config.multiversePath;
              # Lingering starts before a login shell can supply the user's PATH.
              Environment = [ "PATH=${config.home.profileDirectory}/bin:/run/current-system/sw/bin" ];
              # Ignore inherited terminal breadcrumbs when continuing the dedicated session.
              UnsetEnvironment = [
                "ZELLIJ_PANE_ID"
                "TMUX_PANE"
                "CMUX_SURFACE_ID"
                "KITTY_WINDOW_ID"
                "WEZTERM_PANE"
                "TERM_SESSION_ID"
                "WT_SESSION"
              ];
              ExecStart = toString start;
              # The socket keeps the FIFO open, so the frame queues until RPC reads it.
              ExecStartPost = "${nameSession} %t/omp-multiverse/stdin";
              StateDirectory = "omp-multiverse";
              StateDirectoryMode = "0700";
              UMask = "0077";
              StandardInput = "socket";
              Sockets = [ "omp-multiverse.socket" ];
              StandardOutput = "journal";
              StandardError = "journal";
              # OMP's signal cleanup exits with 128 + SIGTERM.
              SuccessExitStatus = [ 143 ];
              Restart = "always";
              RestartSec = 5;
            };
            Install.WantedBy = [ "default.target" ];
          };
        };
    };
}
