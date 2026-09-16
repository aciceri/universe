# OMP collaboration: remote voice and headless hosting

Ten patches extend OMP collaboration. Patches 1 through 8 let a collab guest,
such as a phone or a browser on another machine, hold OMP's `/live` realtime
voice call. The host keeps the Codex WebRTC peer, its credentials and its device
attestation; only the microphone and speaker move to the guest.
Patch 9 adds headless RPC hosting. Patch 10 connects headless hosts to the same
remote live voice path, without requiring terminal components or local audio.

`modules/agents.nix` applies them with `git apply`, **after**
`omp-session-gateway.ompPatch`, because they are cut against the gateway-patched
tree, not pristine upstream:

| | |
|---|---|
| upstream tag | `v18.1.15` (`a33cc26824e3c91edd9fa42d681f10dceb4ac2f0`) |
| gateway mbox | `patches/oh-my-pi/0001-collab-controller-autostart-registry.patch` from `omp-session-gateway` 0.3.0-unstable-2026-09-09 |
| base tree | upstream + that mbox, kept as the git worktree `~/projects/oh-my-pi-gateway-base` |

Rebasing onto the gateway tree was not cosmetic. The mbox moves `CollabHost`
ownership out of the `/collab` slash command into a shared `CollabController`,
so the bridge registration lives in `CollabController.#start` (before
`host.start`, since a guest can claim audio as soon as it joins) and `HostLike`
carries an optional `setLiveAudioBridge`. Cut against pristine upstream, patch
0003 conflicts in `interactive-mode.ts`, `types.ts` and
`builtin-collaboration.ts`, and 0005 cascades from it.

## What each patch does

1. `pi-voice`: tap the decoded live output and make local playback optional.
   `LiveCallbacks::output` receives each frame as mono f32 at 48 kHz;
   `local_playback: false` skips `PlaybackStream` entirely, so the peer touches
   no audio device. The RMS level callback is unaffected — it was already
   computed separately from playback.
2. `coding-agent`: `LiveAudioEndpoint` on `LiveSessionController`.
   `localCapture`/`localPlayback` default to true and preserve current
   behaviour; `pushInput()` feeds externally captured 16 kHz frames. Whether
   the echo gate applies to them is decided by patch 7.
3. `collab`: four additive frames — `live-ctl`, `live-audio` guest-to-host,
   `live-state`, `live-out` host-to-guest — routed through a
   `CollabLiveAudioBridge`. `COLLAB_PROTO` stays 3: both sides already ignore
   unknown frame types. Silent output frames are dropped; the remote track runs
   for the whole call and is almost entirely silence.
4. `collab-web`: microphone capture resampled to 16 kHz, jitter-buffered
   playback, and a microphone button hidden for read-only guests.
5. `collab`: mirror the live transcript and phase to the owning guest, rendered
   in a panel above the composer rather than inside the session transcript —
   the lines are volatile recognition and are never persisted.
6. `collab-web`: interface scale control. Every shell size is hardcoded px, so
   the app frame takes `zoom` and divides its height by the scale to stay one
   viewport tall.
7. `coding-agent`: optional echo gate for remote input. An echo-cancelling
   capture makes Chrome pin output to the platform's communication stream,
   whose ceiling is low on Android and unreachable from a page; the only escape
   is capturing without cancellation, which then needs the echo handled
   host-side. The gate local capture already used is extracted as
   `#echoGated()` and applied to `pushInput` on request, through new `live-ctl`
   ops `gate-on`/`gate-off`. Default off, since an endpoint that cancels echo
   itself would only lose genuine barge-in.
8. `coding-agent`: seed the session state into the realtime instructions. A
   realtime session starts with no history — the transcript lives in the
   `AgentSession`, reachable only through delegations — so "where were we" was
   unanswerable even though the backend knew. A bounded recap (session name,
   open todos, last assistant answer clipped to 600 characters) costs ~530
   characters per call and is omitted entirely when there is nothing to report.
   Uses `{{#if recap}}`: a handlebars section rebinds the context to the
   string, and `{{recap}}` inside it would then resolve to nothing.
9. `collab`: host RPC sessions through the existing `CollabController` when
   `collab.autoStart` is enabled. Narrow structural contexts replace the TUI
   dependency without fabricating terminal components. Supported dialogs use
   the existing encrypted control-guest UI and publish `inputRequired`; session
   replacement rotates ownership, and title/model changes refresh metadata.
   Startup and terminal host failures exit for supervisor recovery; EOF and
   signal cleanup stop publication and dispose the session. Default-off RPC,
   gateway authentication, guest permissions, and the phone client are unchanged.
10. `collab`: register an RPC live-audio bridge before headless publication.
    It uses the existing `LiveSessionController`, configured voice, Codex
    authentication, device attestation, recap and delegation, with local capture
    and playback disabled. Pending-call cancellation, disconnects, session
    replacement and shutdown drain the controller before allowing another call.
    Host ownership is generation-guarded: duplicate claims preserve mute state,
    and late callbacks cannot reclaim ownership or affect a replacement call.
    The gateway, phone client, wire protocol and interactive audio defaults are
    unchanged.

## The browser half lives elsewhere

`omp-session-gateway` vendors its own copy of `packages/collab-web`, and that
copy — not omp's — is what the phone loads. Patches 4 and 6 therefore reach
omp's client only; the gateway's needs
`patches/omp-session-gateway/0001-collab-client-re-vendor-*.patch`, which
re-vendors that copy from this series and carries the browser-side work the
phone actually needs: speaker mode, microphone picker, playback boost, visible
failure text. Both move together.

## Bumping omp

The gateway mbox is cut against an exact upstream tag, so a version bump that
the mbox rejects means waiting for the gateway to reroll. Once it does, rebase
this series on the new gateway-patched tree and re-export — do not rebase it on
pristine upstream.

## Verification

- no audio backend at all: `localPlayback=false` still produces an SDP offer,
  `true` fails with `Failed to open the default speaker`;
- locally synthesized speech pushed through `pushInput` comes back as the
  service's own transcript, and the reply arrives through the output tap;
- a headless collab guest claims audio over `wss://`, streams that speech and
  receives non-silent `live-out` frames plus streaming `live-transcript` and
  `live-phase` frames;
- silence gating moved 51 KB where the raw track would have moved 2.9 MB for
  the same reply.

For patch 9, verification used the built Nix package and runtime-only copies of
the pike service/socket with isolated state:

- the existing phone directory discovered the Opus 5 session; a real control
  guest sent a prompt, received streaming output, aborted a turn, and completed
  an actual `ask` tool interaction;
- selection, editor and confirmation answers reached the headless runtime;
  pending dialogs replayed to a later control guest, cancellation removed the
  dialog, and `inputRequired` cleared;
- a forced process crash restarted automatically with the same session ID and
  persisted assistant response, visible again after reconnecting the phone;
- `new_session` rotated the published generation and session identity;
  `switch_session` restored the original conversation and rotated ownership again;
- explicit stop left both service and socket inactive, removed the FIFO and
  registry entry, and did not reactivate after the restart interval;
- unreachable-relay startup exited unsuccessfully; ordinary RPC remained
  collaboration-free with `ask` unavailable; collaboration-enabled stdin EOF
  exited successfully and removed publication;
- 50 focused controller, read-only guest, guest UI, event-bus fallback and RPC UI
  regression tests passed; the Nix package built and generated units passed
  `systemd-analyze --user verify`.

For patch 10, verification used both the patched source and the built Nix package:

- an isolated RPC host ran without a terminal and without usable local ALSA or
  PulseAudio devices;
- a phone-sized browser joined through the unchanged gateway, streamed locally
  synthesized speech, displayed its real transcription and received the spoken
  reply through the existing client playback path;
- mute, unmute, call release and a second call succeeded; SIGTERM during the
  second connected call shut down the packaged host with exit status 143;
- default-off RPC emitted `ready`, started no collaboration host and exited
  successfully on stdin EOF;
- 56 focused collaboration, RPC UI and live-controller tests passed, including
  pending mute, duplicate claims, disconnects, stale callbacks and awaited
  teardown.

The source-tree type check with the checked-in native declarations reported the
same three `live/transport.ts:185-186` diagnostics before and after patches 9
and 10. Neither headless patch introduced additional diagnostics.
