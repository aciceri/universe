# Remote live voice for omp collab

Eight patches that let a collab guest — a phone, a browser on another machine —
hold omp's `/live` realtime voice call. The host keeps the Codex WebRTC peer,
its credentials and its device attestation; only the microphone and speaker
move to the guest.

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
