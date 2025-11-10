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
Built on a [Radxa Rock5B](https://wiki.radxa.com/Rock5/hardware/5b)—an ARM single-board computer
featuring a Rockchip RK3588, 16GB of DDR4 memory, and 2.5GBit ethernet connectivity.
Storage consists of an SATA SSD for the OS and an enterprise-grade 12TB HDD for data,
connected via an NVMe-to-SATA adapter.

#### Web services

##### Public

- [aciceri.dev](https://aciceri.dev)
- [agenix-shell.talks.aciceri.dev](https://agenix-shell.talks.aciceri.dev)
- [auth.aciceri.dev](https://auth.aciceri.dev)
- [blog.aciceri.dev](https://blog.aciceri.dev)
- [cal.aciceri.dev](https://cal.aciceri.dev)
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
- [atuin.sisko.wg.aciceri.dev](https://atuin.sisko.wg.aciceri.dev)
- [cal.sisko.wg.aciceri.dev](https://cal.sisko.wg.aciceri.dev)
- [firefox-sync.sisko.wg.aciceri.dev](https://firefox-sync.sisko.wg.aciceri.dev)
- [lidarr.sisko.wg.aciceri.dev](https://lidarr.sisko.wg.aciceri.dev)
- [paper.sisko.wg.aciceri.dev](https://paper.sisko.wg.aciceri.dev)
- [prowlarr.sisko.wg.aciceri.dev](https://prowlarr.sisko.wg.aciceri.dev)
- [radarr.sisko.wg.aciceri.dev](https://radarr.sisko.wg.aciceri.dev)
- [sonarr.sisko.wg.aciceri.dev](https://sonarr.sisko.wg.aciceri.dev)
- [status.sisko.wg.aciceri.dev](https://status.sisko.wg.aciceri.dev)
- [torrent.sisko.wg.aciceri.dev](https://torrent.sisko.wg.aciceri.dev)
- [trilium.sisko.wg.aciceri.dev](https://trilium.sisko.wg.aciceri.dev)

### Pike

My mobile workstation—an Acer Nitro V15 laptop equipped with an i5-13420H CPU,
32GB of DDR5 RAM, and an Nvidia RTX 2050 GPU.
Configured to closely mirror my main workstation `picard` for seamless work when away from home.

### Picard

My main workstation—a custom-built powerhouse featuring an AMD Ryzen 9 7900X,
64GB of DDR5 RAM, and blazing-fast NVMe storage.

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

### Curriculum Vitae

My CV is built using LaTeX and continuously served at https://cv.aciceri.dev
The project is stored under [projects/cv](projects/cv).

To build the served derivation, run:

```bash
  nix build .#cv
```

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

### [asciinema-player](packages/asciinema-player/_package.nix) (version 3.12.1)

A web player for terminal session recordings

### [helix-master](packages/helix-master/_package.nix) (version master-a79292b630ae4a0e6e37814ad21411ab50926c73)

Post-modern modal text editor

### [hyphenopoly](packages/hyphenopoly/_package.nix) (version 6.0.0)

Hyphenation for node and Polyfill for client-side hyphenation

### [mirror-checks](packages/mirror-checks/_package.nix) (version 1.0.0)

Utility to synchronize CI checks from Forgejo to GitHub

### [reinstall-magisk-on-lineageos](packages/reinstall-magisk-on-lineageos/_package.nix) (version 1ca911e)

Small bash script to reinstall magisk after each LineageOS update

### [yt-dlp-master](packages/yt-dlp-master/_package.nix) (version 2025.10.22-unstable-2025-11-10)

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
