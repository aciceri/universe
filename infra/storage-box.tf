# The 1 TB Storage Box that sisko backs up to over SFTP (modules/restic.nix).
# It predates this project and holds ~640 GiB of backups, hence the import
# below and the belt-and-braces lifecycle block.
#
# `password` is write-only as far as the API is concerned: it is never returned,
# so an import leaves it unset in the state and every plan would want to write
# it back. The current password predates Hetzner's password policy and the API
# now rejects it (422, "must contain at least one upper case letter, ..."), so
# the password stays managed out of band and OpenTofu is told to leave it be.
resource "hcloud_storage_box" "backups" {
  name             = "storage-box-366463"
  storage_box_type = "bx11"
  location         = "fsn1"
  password         = var.storage_box_password

  access_settings = {
    reachable_externally = true
    ssh_enabled          = true
    samba_enabled        = false
    webdav_enabled       = false
    zfs_enabled          = false
  }

  lifecycle {
    prevent_destroy = true

    ignore_changes = [
      # Rotating it here would take a policy-conforming password *and* a
      # simultaneous update of the restic secret; not worth the coupling.
      password,
      # The API cannot update the injected keys, so any drift here would be
      # reported as "requires replacement" — i.e. as deleting the backups.
      ssh_keys,
    ]
  }
}

# janeway backs up here, but it is the only machine in the fleet exposed to the
# internet: a subaccount jailed to its own directory means a compromise there
# cannot read or delete sisko's backups.
resource "hcloud_storage_box_subaccount" "janeway" {
  storage_box_id = hcloud_storage_box.backups.id
  home_directory = "janeway"
  password       = var.storage_box_janeway_password
  description    = "restic repository of janeway"

  access_settings = {
    reachable_externally = true
    ssh_enabled          = true
    samba_enabled        = false
    webdav_enabled       = false
    readonly             = false
  }
}

import {
  to = hcloud_storage_box.backups
  id = "366463"
}

# Already uploaded to the project before this repository managed it: the
# for_each key is substr(sha256(<public key>), 0, 8) of my ed25519 key.
import {
  to = hcloud_ssh_key.ccr["031a87bc"]
  id = "103260018"
}
