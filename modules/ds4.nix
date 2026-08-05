{
  # DwarfStar (ds4) inference engine + always-on ds4-server launchd agent
  # exposing an OpenAI-compatible API on localhost.
  #
  # The model (an ~81GB GGUF, not managed by nix) is fetched automatically by
  # the ds4-download oneshot agent: it exits immediately when the model is
  # already there, otherwise downloads it (curl resumes partial downloads, and
  # launchd retries on failure). ds4-server only runs while the model file
  # exists (KeepAlive.PathState), so nothing crash-loops before the download
  # completes.
  #
  # Idle cost of the server is negligible: the GGUF is mmap'd read-only and
  # faulted lazily, so pages become resident only when inference touches them,
  # and macOS can reclaim them under memory pressure. What persists is the KV
  # cache/scratch, sized by `ctx`.
  flake.modules.darwin.ds4 =
    {
      config,
      lib,
      pkgs,
      ...
    }:
    let
      cfg = config.services.ds4-server;
      home = config.users.users.${config.system.primaryUser}.home;

      downloadScript = pkgs.writeShellScript "ds4-download" ''
        [ -e ${lib.escapeShellArg cfg.model} ] && exit 0

        # Guard against a concurrent manual ds4-download-model run: two curls
        # appending to the same .part file would corrupt it. Non-zero exit so
        # launchd retries later. The pid file lets us reclaim a lock left
        # behind by a SIGKILLed run (trap does not fire on SIGKILL).
        lock=/tmp/ds4-download.lock
        if ! mkdir "$lock" 2>/dev/null; then
          pid=$(cat "$lock/pid" 2>/dev/null || true)
          if [ -n "$pid" ] && kill -0 "$pid" 2>/dev/null; then
            echo "ds4-download: another download holds $lock, retrying later" >&2
            exit 1
          fi
          echo "ds4-download: removing stale $lock" >&2
          rm -rf "$lock"
          mkdir "$lock" || exit 1
        fi
        echo $$ > "$lock/pid"
        trap 'rm -rf "$lock"' EXIT

        DS4_ROOT=${lib.escapeShellArg (dirOf cfg.model)} \
          ${lib.getExe' pkgs.ds4 "ds4-download-model"} ${lib.escapeShellArg cfg.quant}

        # PathState is checked lazily by launchd; kick the server so it comes
        # up as soon as the model is in place.
        /bin/launchctl kickstart "gui/$(id -u)/org.nixos.ds4-server" || true
      '';
    in
    {
      options.services.ds4-server = {
        model = lib.mkOption {
          type = lib.types.str;
          default = "${home}/.local/share/ds4/ds4flash.gguf";
          description = ''
            Path to the GGUF model. Keep the ds4flash.gguf filename: it's the
            symlink ds4-download-model maintains, and its appearance is what
            marks the download as complete.
          '';
        };
        quant = lib.mkOption {
          type = lib.types.str;
          default = "q2-imatrix";
          description = "download_model.sh target fetched when the model is missing.";
        };
        port = lib.mkOption {
          type = lib.types.port;
          default = 8000;
        };
        ctx = lib.mkOption {
          type = lib.types.int;
          # Upstream's canonical server/agent invocation; DeepSeek V4's
          # compressed KV keeps large contexts cheap.
          default = 100000;
          description = "Context window; determines KV cache memory kept by the server.";
        };
        extraArgs = lib.mkOption {
          type = with lib.types; listOf str;
          default = [ ];
          # --kv-disk-dir speeds up resuming agent sessions but persists
          # conversation text in cleartext inside the .kv files: opt-in.
          example = [
            "--nothink"
            "--kv-disk-dir"
            "/tmp/ds4-kv"
            "--kv-disk-space-mb"
            "8192"
          ];
        };
      };

      config = {
        environment.systemPackages = [ pkgs.ds4 ];

        launchd.user.agents.ds4-download.serviceConfig = {
          ProgramArguments = [ "${downloadScript}" ];
          RunAtLoad = true;
          # Retry while it fails (network, lock); stop for good on success.
          KeepAlive.SuccessfulExit = false;
          StandardOutPath = "${home}/Library/Logs/ds4-download.log";
          StandardErrorPath = "${home}/Library/Logs/ds4-download.log";
        };

        launchd.user.agents.ds4-server.serviceConfig = {
          ProgramArguments = [
            (lib.getExe' pkgs.ds4 "ds4-server")
            "-m"
            cfg.model
            "--host"
            "127.0.0.1"
            "--port"
            (toString cfg.port)
            "--ctx"
            (toString cfg.ctx)
          ]
          ++ cfg.extraArgs;
          RunAtLoad = true;
          # Alive only while the model exists.
          KeepAlive.PathState.${cfg.model} = true;
          StandardOutPath = "${home}/Library/Logs/ds4-server.log";
          StandardErrorPath = "${home}/Library/Logs/ds4-server.log";
        };
      };
    };
}
