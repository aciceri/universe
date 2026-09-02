{
  readme.parts.nixos = ''
    ### Janeway

    A Hetzner Cloud `cx23` in Nuremberg, created and destroyed by the
    [OpenTofu project](#infrastructure) rather than by hand. Its only job is
    running [simple-nixos-mailserver](https://nixos-mailserver.readthedocs.io)
    for `ciceri.me`, so it imports the bare `server` profile and nothing else:
    no docker, no zerotier, no stylix, no desktop leftovers.

    - `mail.ciceri.me` is the SMTP/IMAP endpoint and the name in the PTR records
    - `webmail.ciceri.me` is [Roundcube](modules/roundcube.nix), talking to the
      same endpoint over IMAPS/submissions rather than over localhost
    - `janeway.ciceri.me` resolves to the public address; `janeway.wg.aciceri.dev`
      to `10.100.0.9` on the mesh, which is what deploys go through
    - being on the mesh it also pulls from `ncps` on sisko, ships its journal
      to Loki and is scraped by prometheus like every other host

    Its root filesystem is a tmpfs, so
    [`persist.nix`](hosts/janeway/persist.nix) is the complete and honest list
    of the state it owns. That state goes to a dedicated Storage Box
    subaccount every night ([`restic.nix`](modules/restic.nix)) — a subaccount
    of its own because this is the only machine in the fleet reachable from
    the internet, and a compromise here must not reach sisko's backups.

    There is deliberately **no server-side expiry of old mail**. IMAP
    synchronises rather than downloads, so anything deleted here also
    disappears from every client that had already fetched it; thinning out the
    mailbox is a client-side job (move to local folders). The disk is watched
    instead: [`disk-alert.nix`](modules/disk-alert.nix) mails a warning past
    80%, and the `server` profile garbage-collects nix generations weekly,
    which is what actually fills 40 GB.
  '';
}
