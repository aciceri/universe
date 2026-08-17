This repository is my comprehensive monorepo containing personal projects
and configurations that aren't intended for external contributions,
though contributions are always welcome.

It follows the [dendritic pattern](https://github.com/mightyiam/dendritic),
meaning each file is a [flake-parts](https://flake.parts/) module.
Also, this README is dynamically generated using a flake-parts module, with its
source distributed throughout the repository.

## NixOS configurations

As you can notice I'm a big Star Trek fan...

### Sisko

My homeserver hosting a wide variety of private and public services.
Running on my decommissioned Thinkpad Carbon X1 gen 7

#### Web services

##### Public

- [aciceri.dev](https://aciceri.dev)
- [agenix-shell.talks.aciceri.dev](https://agenix-shell.talks.aciceri.dev)
- [auth.aciceri.dev](https://auth.aciceri.dev)
- [blog.aciceri.dev](https://blog.aciceri.dev)
- [cal.aciceri.dev](https://cal.aciceri.dev)
- [claude.aciceri.dev](https://claude.aciceri.dev)
- [cloud.aciceri.dev](https://cloud.aciceri.dev)
- [cv.aciceri.dev](https://cv.aciceri.dev)
- [git.aciceri.dev](https://git.aciceri.dev)
- [home.aciceri.dev](https://home.aciceri.dev)
- [jelly.aciceri.dev](https://jelly.aciceri.dev)
- [nixos-devops.talks.aciceri.dev](https://nixos-devops.talks.aciceri.dev)
- [nixos-tests.talks.aciceri.dev](https://nixos-tests.talks.aciceri.dev)
- [photos.aciceri.dev](https://photos.aciceri.dev)

##### WireGuard

- [adguard.sisko.wg.aciceri.dev](https://adguard.sisko.wg.aciceri.dev)
- [amule.sisko.wg.aciceri.dev](https://amule.sisko.wg.aciceri.dev)
- [cal.sisko.wg.aciceri.dev](https://cal.sisko.wg.aciceri.dev)
- [calibre.sisko.wg.aciceri.dev](https://calibre.sisko.wg.aciceri.dev)
- [claude.sisko.wg.aciceri.dev](https://claude.sisko.wg.aciceri.dev)
- [collab.sisko.wg.aciceri.dev](https://collab.sisko.wg.aciceri.dev)
- [cups.sisko.wg.aciceri.dev](https://cups.sisko.wg.aciceri.dev)
- [food.sisko.wg.aciceri.dev](https://food.sisko.wg.aciceri.dev)
- [lidarr.sisko.wg.aciceri.dev](https://lidarr.sisko.wg.aciceri.dev)
- [omp.sisko.wg.aciceri.dev](https://omp.sisko.wg.aciceri.dev)
- [paper.sisko.wg.aciceri.dev](https://paper.sisko.wg.aciceri.dev)
- [prowlarr.sisko.wg.aciceri.dev](https://prowlarr.sisko.wg.aciceri.dev)
- [radarr.sisko.wg.aciceri.dev](https://radarr.sisko.wg.aciceri.dev)
- [sonarr.sisko.wg.aciceri.dev](https://sonarr.sisko.wg.aciceri.dev)
- [status.sisko.wg.aciceri.dev](https://status.sisko.wg.aciceri.dev)
- [torrent.sisko.wg.aciceri.dev](https://torrent.sisko.wg.aciceri.dev)
- [trilium.sisko.wg.aciceri.dev](https://trilium.sisko.wg.aciceri.dev)
- [vault.sisko.wg.aciceri.dev](https://vault.sisko.wg.aciceri.dev)

### Pike

My mobile workstation—an Acer Nitro V15 laptop equipped with an i5-13420H CPU,
32GB of DDR5 RAM, and an Nvidia RTX 2050 GPU.
Configured to closely mirror my main workstation `picard` for seamless work when away from home.

### Picard

My main workstation—a custom-built powerhouse featuring an AMD Ryzen 9 7900X,
64GB of DDR5 RAM, and blazing-fast NVMe storage.

### Kirk

A NixOS desktop VM (`aarch64-linux`) running on Archer via QEMU with
GPU acceleration (virgl over Metal).

## nix-darwin configurations

### Archer

My MacBook Pro (Apple Silicon, `aarch64-darwin`), managed via nix-darwin.

## Projects

### NixOS testing framework talk

Talk about the NixOS tests framework for 2025 Milan Linux Day.

#### Running the Slides

```bash
pnpm install  # Install dependencies
pnpm dev      # Start development server
```

This will start a development server for the presentation slides.

#### Building

```bash
pnpm build                        # Build for production
nix build .#nixos-tests-talk      # Build presentation as a completely self-contained derivation
```

### "NixOS per DevOps" talk

Talk introducing NixOS for DevOps, in italian.
[Here](https://www.youtube.com/watch?v=dH3_H2ixvzg) the recording.

### `agenix-shell` talk

A presentation about [agenix-shell](https://github.com/aciceri/agenix-shell) for a 5-minute flash talk at NixCon 2025.
[Here](https://www.youtube.com/watch?v=pE3wha4jlos) the recording.

#### Running the Slides

```bash
pnpm install  # Install dependencies
pnpm dev      # Start development server
```

This will start a development server for the presentation slides.

#### Building

```bash
pnpm build                        # Build for production
nix build .#agenix-shell-talk     # Build presentation as a completely self-contained derivation
```

### Foodlog

AI-powered progressive web app for tracking food intake.

### Curriculum Vitae

My CV is built using LaTeX and continuously served at https://cv.aciceri.dev
The project is stored under [projects/cv](projects/cv).

To build the served derivation, run:

```bash
  nix build .#cv
```

### CalTrack

Simple Python tool to extract billable hours from my calendar.
In a future it may be extended to directly create invoices.

### Blog

My blog is reachable at https://blog.aciceri.dev, the website is continuously deployed.
The project is stored under [projects/blog](projects/blog).

#### Development

Enter the project directory and development shell to access Cabal and dependencies.

To build the served derivation, run:

```bash
  nix build .#blog
```

To start a development server with hot reload, run:

```bash
  nix run .#blog.watch
```

## NUR packages

This repository also acts as a NUR repository. Note that not all packages in the `packages`
flake output are included in the [\_nur.nix file](packages/_nur.nix).

### [asciinema-player](packages/asciinema-player/_package.nix) (version 3.17.0)

A web player for terminal session recordings

### [catppuccin-gitea](packages/catppuccin-gitea/_package.nix) (version 1.0.2)

Soothing pastel theme for Gitea and Forgejo

### [dms-ccusage-plugin](packages/dms-ccusage-plugin/_package.nix) (version 1.0.0)

DMS plugin showing Claude Code billing block usage

### [ds4](packages/ds4/_package.nix) (version 0-unstable-2026-08-09)

DwarfStar: DeepSeek V4 Flash and PRO local inference engine (Metal build)

### [emacs-hel](packages/emacs-hel/_package.nix) (version new_undo_system-unstable-2026-08-16)

Helix emulation layer for Emacs

### [emacs-hel-collection](packages/emacs-hel-collection/_package.nix) (version 0-unstable-2026-08-07)

Collection of Hel keybindings for built-in and third-party Emacs packages

### [emacs-hel-ghostel](packages/emacs-hel-ghostel/_package.nix) (version 0.3.0-unstable-2026-07-09)

Hel integration for the Ghostel terminal emulator

### [emacs-hel-leader](packages/emacs-hel-leader/_package.nix) (version 2.1-unstable-2026-07-09)

Leader key for Hel, the Emacs Helix emulation layer

### [emacs-hel-org](packages/emacs-hel-org/_package.nix) (version 0-unstable-2026-07-09)

Hel integration with Org mode

### [emacs-mcp-server](packages/emacs-mcp-server/_package.nix) (version 0.7.0-unstable-2026-05-04)

Pure Elisp MCP server exposing Emacs to LLM agents

### [emacs-niri-awareness](packages/emacs-niri-awareness/_package.nix) (version 0-unstable-2026-07-31)

Emacs IPC client for the niri Wayland compositor

### [emacs-terraform-ts-mode](packages/emacs-terraform-ts-mode/_package.nix) (version 0.6-unstable-2026-05-31)

Terraform major mode for Emacs using tree-sitter and eglot

### [emacs-vertico-buffer-frame](packages/emacs-vertico-buffer-frame/_package.nix) (version 0-unstable-2026-06-12)

Child-frame display for Vertico completions

### [hass-garmin-connect](packages/hass-garmin-connect/_package.nix) (version 3.0.15-unstable-2026-08-11)

Home Assistant integration exposing and uploading Garmin Connect data

### [hass-pun-sensor](packages/hass-pun-sensor/_package.nix) (version 4.0.1-unstable-2026-04-18)

Home Assistant integration exposing the Italian PUN electricity prices

### [helix-master](packages/helix-master/_package.nix) (version master-079a789e8cb08ead67f19e1971a1b7438b37354b)

Post-modern modal text editor

### [hyphenopoly](packages/hyphenopoly/_package.nix) (version 6.1.0)

Hyphenation for node and Polyfill for client-side hyphenation

### [mirror-checks](packages/mirror-checks/_package.nix) (version 1.0.0)

Utility to synchronize CI checks from Forgejo to GitHub

### [omp-collab-dashboard](packages/omp-collab-dashboard/_package.nix) (version 1.0.0)

Dashboard listing active omp collab sessions

### [omp-collab-relay](packages/omp-collab-relay/_package.nix) (version 1.0.0)

Self-hosted relay for omp collab E2E-encrypted session sharing

### [reinstall-magisk-on-lineageos](packages/reinstall-magisk-on-lineageos/_package.nix) (version 1ca911e)

Small bash script to reinstall magisk after each LineageOS update

### [yt-dlp-master](packages/yt-dlp-master/_package.nix) (version 2026.07.04-unstable-2026-08-17)

yt-dlp is a youtube-dl fork based on the now inactive youtube-dlc.

youtube-dl is a small, Python-based command-line program
to download videos from YouTube.com and a few more sites.
youtube-dl is released to the public domain, which means
you can modify it, redistribute it or use it however you like.

## Autogenerated files

The following files are automatically generated:

- [.forgejo/workflows/build-checks.yaml](.forgejo/workflows/build-checks.yaml)
- [.gitignore](.gitignore)
- [README.md](README.md)
- [packages/\_nur.nix](packages/_nur.nix)
