# Unattended daily work brief for the geosurge workspace.
#
# Runs omp headless every weekday morning against ~/geosurge; the prompt and
# the read-only MCP enforcement live in the workspace itself
# (geosurge/automation/daily-brief.md and daily-brief-config.yml).
#
# Cross-platform: launchd agent on darwin, systemd user timer on NixOS.
# Importing hosts must also declare the webhook secret (see
# hosts/archer/secrets.nix) and rekey it for their host key.
{
  flake.modules.homeManager.daily-brief =
    {
      pkgs,
      lib,
      config,
      osConfig,
      ...
    }:
    let
      workspace = "${config.home.homeDirectory}/geosurge";
      logDir = "${config.home.homeDirectory}/Library/Logs";
      runBrief = pkgs.writeShellScript "daily-brief" ''
        # launchd/systemd start agents with a minimal environment: seed PATH so
        # omp's bash tool can find gh/git/nix. Covers both layouts: darwin HM
        # installs user packages into ~/.nix-profile (no /etc/profiles), NixOS
        # into /etc/profiles/per-user.
        export PATH="$HOME/.nix-profile/bin:/etc/profiles/per-user/$USER/bin:/run/current-system/sw/bin:/usr/bin:/bin"

        # Invoked both by the 08:30 calendar/timer trigger and at load (login,
        # boot catch-up, agent reload). Guard: at most one run per day, and
        # never before 08:30 (an early boot waits for the scheduled trigger).
        # For manual runs, --force skips the guard.
        state_dir="$HOME/.local/state/daily-brief"
        stamp="$state_dir/last-run"
        today=$(date +%F)
        if [ "''${1:-}" != "--force" ]; then
          [ "$(cat "$stamp" 2>/dev/null)" = "$today" ] && exit 0
          [ "$(date +%H%M)" -lt 0830 ] && exit 0
          [ "$(date +%u)" -gt 5 ] && exit 0  # weekdays only, like the scheduled trigger
        fi
        mkdir -p "$state_dir"

        # Sync the workspace metadata repo with GitHub before the run: clone
        # on first use (turnkey on a new host), otherwise rebase local
        # cash-register commits on top of the remote. Offline or conflicting?
        # Abort the rebase and run against the local state — the next
        # successful sync reconciles. accept-new: non-interactive first
        # contact with github.com from a fresh host.
        export GIT_SSH_COMMAND="ssh -o StrictHostKeyChecking=accept-new"
        if [ ! -d "${workspace}/.git" ]; then
          ${pkgs.git}/bin/git clone git@github.com:aciceri/geosurge.git "${workspace}" \
            || { echo "workspace clone failed"; exit 1; }
        else
          ${pkgs.git}/bin/git -C "${workspace}" pull --rebase --autostash \
            || { ${pkgs.git}/bin/git -C "${workspace}" rebase --abort 2>/dev/null; echo "git pull failed; running on local state"; }
        fi

        echo "=== daily-brief run $(date '+%F %T') ==="
        # Transient failures happen (e.g. token refresh timing out right after
        # boot): retry a few times, and stamp the day only on success so a
        # failed morning run gets retried at the next login trigger.
        ok=0
        for attempt in 1 2 3; do
          if ${pkgs.llm-agents.omp}/bin/omp \
            --cwd "${workspace}" \
            --config "${workspace}/automation/daily-brief-config.yml" \
            --max-time 900 \
            -p @"${workspace}/automation/daily-brief.md"; then
            ok=1
            break
          fi
          echo "omp run failed (attempt $attempt)"
          sleep $((attempt * 30))
        done
        if [ "$ok" != 1 ]; then
          echo "daily-brief failed after 3 attempts; not stamping $today"
          exit 1
        fi
        printf '%s' "$today" > "$stamp"

        # Post today's journal to Slack via incoming webhook (hard-scoped to
        # one channel by construction — the agent session stays read-only).
        # Skips quietly until the secret exists.
        webhook_file="${osConfig.age.secrets.slack_webhook_daily_brief.path}"
        journal="${workspace}/status/journal/$today.md"
        if [ -r "$webhook_file" ] && [ -f "$journal" ]; then
          # Slack speaks mrkdwn, not markdown: escape &<>, headers/**bold** ->
          # *bold*, [t](url) -> <url|t>, "- " lists -> bullets. Backticks match.
          ${pkgs.gnused}/bin/sed -E \
            -e 's/&/\&amp;/g' -e 's/</\&lt;/g' -e 's/>/\&gt;/g' \
            -e 's/^#{1,6} (.*)$/*\1*/' \
            -e 's/\*\*([^*]+)\*\*/*\1*/g' \
            -e 's/\[([^]]+)\]\(([^)]+)\)/<\2|\1>/g' \
            -e 's/^- /• /' \
            -e 's/^(  +)- /\1◦ /' \
            "$journal" \
            | ${pkgs.jq}/bin/jq -Rs '{text: .}' \
            | ${pkgs.curl}/bin/curl -sf -X POST -H 'Content-Type: application/json' -d @- "$(cat "$webhook_file")" \
            || echo "slack webhook post failed"
        else
          echo "slack webhook skipped (secret or journal missing)"
        fi

        # Cash-register commit of the workspace metadata repo (STATUS.md,
        # journal, AGENTS.md, automation/): history + rollback for state the
        # job rewrites. Inner repos are excluded by the workspace .gitignore.
        # Push closes the daily sync loop opened by the pull above; a failed
        # push (offline) is fine, the next run's rebase carries it forward.
        ${pkgs.git}/bin/git -C "${workspace}" add -A
        ${pkgs.git}/bin/git -C "${workspace}" commit -qm "daily-brief $(date '+%F %T')" \
          || echo "git commit skipped (no changes)"
        ${pkgs.git}/bin/git -C "${workspace}" push \
          || echo "git push failed (will retry next run)"
      '';
    in
    lib.mkMerge [
      {
        # Same entrypoint for manual runs; --force reruns even if today already ran.
        programs.nushell.shellAliases.daily-brief = "${runBrief} --force";

        # Interactive omp session in the geosurge workspace from anywhere.
        programs.nushell.shellAliases.geo = "${lib.getExe pkgs.llm-agents.omp} --cwd ${workspace}";
      }

      (lib.mkIf pkgs.stdenv.hostPlatform.isDarwin {
        launchd.agents.daily-brief = {
          enable = true;
          config = {
            ProgramArguments = [ (toString runBrief) ];
            # Weekdays at 08:30; RunAtLoad + the stamp guard in the script cover
            # boots/logins after 08:30 (powered-off machines miss calendar
            # triggers; sleeping ones get them coalesced at wake).
            RunAtLoad = true;
            StartCalendarInterval =
              map
                (weekday: {
                  Weekday = weekday;
                  Hour = 8;
                  Minute = 30;
                })
                [
                  1
                  2
                  3
                  4
                  5
                ];
            StandardOutPath = "${logDir}/daily-brief.log";
            StandardErrorPath = "${logDir}/daily-brief.err";
          };
        };
      })

      (lib.mkIf pkgs.stdenv.hostPlatform.isLinux {
        # systemd analog of the launchd agent above; logs land in the user
        # journal (journalctl --user -u daily-brief).
        systemd.user.services.daily-brief = {
          Unit.Description = "Daily work brief (omp headless run)";
          Service = {
            Type = "oneshot";
            ExecStart = toString runBrief;
          };
        };
        systemd.user.timers.daily-brief = {
          Unit.Description = "Daily work brief schedule";
          Timer = {
            OnCalendar = "Mon..Fri 08:30";
            # RunAtLoad analog: fire immediately when a missed 08:30 is found
            # at boot/login; the script's stamp guard still caps at one run per
            # day and skips weekends.
            Persistent = true;
          };
          Install.WantedBy = [ "timers.target" ];
        };
      })
    ];
}
