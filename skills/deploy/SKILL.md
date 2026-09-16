---
name: deploy
description: Build, dry run, and deploy the universe NixOS and nix-darwin configurations with nh, locally or to a remote host over SSH.
---

# Deploy

Read this before running any `nh` command against this flake. It covers building, inspecting, and activating the configurations in `universe`, for the local machine and for the remote hosts.

## Authorization

Building and dry running are ordinary work and need no permission. Activating is a deploy: `nh os switch`, `boot`, `test`, and `rollback` all change a real machine, so they require Andrea's explicit authorization for that specific host, as stated in the [multiverse instructions](../../../AGENTS.md). Never activate a configuration to make a verification easier, and never deploy a host that was not named. Propose the exact command instead and let Andrea decide.

A dirty worktree is the normal state here. A switch activates the working tree, uncommitted changes included, so read `git -C universe status --short` first and say which pending changes would go live.

## Hosts

| Host | What it is | Deploy target |
|---|---|---|
| `pike` | this workstation | local, or `root@localhost` over SSH |
| `sisko` | homeserver, most private and public services | `root@sisko.wg.aciceri.dev` |
| `picard` | mesh host | `root@picard.wg.aciceri.dev` |
| `kirk` | mesh host | `root@kirk.wg.aciceri.dev` |
| `janeway` | Hetzner mail server for `ciceri.me` | `root@janeway.wg.aciceri.dev` |
| `archer` | nix-darwin machine, use `nh darwin` | local only |

`nixosConfigurations` holds `janeway`, `kirk`, `picard`, `pike`, and `sisko`; `archer` is the only `darwinConfigurations` entry. Deploys reach the mesh names, not the public ones. Root login is key based and passwordless on the mesh hosts (`PermitRootLogin prohibit-password`), so no password is ever needed for the SSH path below.

`NH_FLAKE` already points at the universe checkout in Andrea's shell, so `-H <host>` alone selects the configuration. Pass the flake path explicitly when the environment is not inherited, for example `nh os switch ~/multiverse/universe -H pike`.

## Readable output for an agent

Always add `--no-nom`. Without it `nh` renders the build with nix-output-monitor, an interactive full screen display that is unreadable once captured. With it the build is plain log lines followed by the generation diff.

Useful additions:

- `--dry` stops after the build and the diff, activating nothing. This is the safe default when reporting what a change would do.
- `-L` prints full build logs, which is what you want when a derivation fails.
- `-d never` suppresses the package diff, `-d always` forces it. The default already prints it for a matching configuration.
- `-a` asks for confirmation interactively, so never use it in automation.
- `-R` is only needed when `nh` itself runs as root, which the recipes below avoid.

The diff is the deliverable. Report the `<<<` old and `>>>` new system store paths, the `CHANGED` and `ADDED` entries, and the closing `PATHS`, `SIZE`, and `DIFF` lines.

## The local machine

`nh` elevates a local activation through `sudo`, which prompts for a password and cannot be driven from a tool call. Deploy `pike` through SSH to its own root account instead, where the key already grants uid 0:

```sh
nh os switch -H pike --target-host root@localhost --no-nom
```

Check first, then activate:

```sh
nh os switch -H pike --target-host root@localhost --no-nom --dry
```

`nh` builds locally, copies the closure with `nix copy --to` (a no-op when the target is the same store), and runs the activation over SSH as root. It passes an argument vector rather than a shell command line, which matters because root's login shell is `nu`: plain argv works, but a POSIX shell one liner typed into `ssh root@...` does not. Write `out+err>` instead of `2>&1` in any manual `ssh root@...` command, or avoid redirection entirely.

`--elevation-strategy` covers the remaining cases: `none` when the session is already root, `passwordless` for a target with `NOPASSWD`, a program path such as `/usr/bin/sudo` to force one. `--install-bootloader` through the `root@localhost` path has not been exercised; run a bootloader install from a real root session.

## A remote host

```sh
nh os switch -H sisko --target-host root@sisko.wg.aciceri.dev --no-nom
```

The same shape works for `picard`, `kirk`, and `janeway` by swapping both the `-H` name and the mesh host. By default the build runs on this machine and only the closure travels.

Build on the target instead when the host owns a large configuration:

```sh
nh os switch -H sisko --target-host root@sisko.wg.aciceri.dev --build-host root@sisko.wg.aciceri.dev --no-nom
```

Sisko's closure is large and its build pulls in service tests and web frontends. Building it locally on `pike` filled the ZFS pool and failed with `No space left on device` from a pool that had about 27 GB free, so check `df -h /nix` before a local build of a server configuration, or push the build to the target or to the workflow below. `NIX_SSHOPTS` is honoured for extra SSH options, and sisko is the `ncps` substituter for the mesh, so deploys of the other hosts mostly download rather than rebuild.

## Continuous deployment

`.forgejo/workflows/deploy.yaml` is a `workflow_dispatch` with a host choice and runs `nixos-rebuild switch --flake .#<host> --target-host root@<host>.wg.aciceri.dev --print-build-logs --fast` from the runner. Prefer that workflow for a routine deploy of committed work, and use the commands above for iteration on something not yet pushed. `build-checks.yaml` is the gate for correctness; a local `nh os build -H <host> --no-nom` is the quick equivalent, subject to the same disk space caveat.

## Inspect and roll back

```sh
nh os info
```

lists the generations of the local machine with build date, version, kernel, and closure size. `nh os rollback` reverts the local machine and accepts `-t <generation>`; it has no `--target-host`, so a remote rollback goes through the host itself, again as plain argv:

```sh
ssh root@sisko.wg.aciceri.dev readlink /nix/var/nix/profiles/system
ssh root@sisko.wg.aciceri.dev /nix/var/nix/profiles/system-90-link/bin/switch-to-configuration switch
```

Read the current generation first, then pick the number below it. Treat a rollback as a deploy for authorization purposes.

## Finish

Report the host, whether the run was a dry run or a real activation, the new system store path, and the diff summary. When a build fails, quote the failing derivation and the relevant log lines rather than the whole output. Do not commit, push, or trigger the Forgejo workflow as part of a deploy; those are separate authorizations.
