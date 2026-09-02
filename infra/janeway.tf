locals {
  # Hetzner location: Nuremberg. cx23 (CX Gen3) is offered in every EU
  # location, and nbg1 sits in the eu-central network zone together with any
  # future sibling.
  location = "nbg1"
}

# Uploaded so the very first boot of a stock image is reachable by SSH, which is
# all nixos-anywhere needs. Once NixOS is installed the authorized keys come
# from the flake instead.
resource "hcloud_ssh_key" "ccr" {
  for_each = { for key in var.ssh_public_keys : substr(sha256(key), 0, 8) => key }

  name       = "ccr-${each.key}"
  public_key = each.value
}

# The addresses are the identity of the mail server: they are published in DNS,
# in the PTR records and in other people's reputation databases. Allocating them
# as standalone Primary IPs with auto_delete = false keeps them across a
# reinstall or a resize of the machine.
resource "hcloud_primary_ip" "janeway_ipv4" {
  name        = "janeway-ipv4"
  type        = "ipv4"
  location    = local.location
  auto_delete = false
}

resource "hcloud_primary_ip" "janeway_ipv6" {
  name        = "janeway-ipv6"
  type        = "ipv6"
  location    = local.location
  auto_delete = false
}

resource "hcloud_server" "janeway" {
  name        = "janeway"
  server_type = "cx23"
  location    = local.location

  # Only ever booted to let nixos-anywhere kexec into the NixOS installer; the
  # real system is whatever `nixos-rebuild switch --flake .#janeway` produces.
  image = "debian-13"

  ssh_keys = [for key in hcloud_ssh_key.ccr : key.id]
  labels   = { managed_by = "opentofu" }

  public_net {
    ipv4_enabled = true
    ipv4         = hcloud_primary_ip.janeway_ipv4.id
    ipv6_enabled = true
    ipv6         = hcloud_primary_ip.janeway_ipv6.id
  }

  lifecycle {
    # Rotating my SSH keys must not destroy the machine: after the install the
    # NixOS configuration owns authorized_keys anyway.
    ignore_changes = [ssh_keys]
  }
}

# Receiving MTAs check that the PTR of the connecting address resolves to the
# name the server announces in EHLO, i.e. mailserver.fqdn. Bound to the Primary
# IPs rather than to the server so it survives a reinstall.
resource "hcloud_rdns" "janeway_ipv4" {
  primary_ip_id = hcloud_primary_ip.janeway_ipv4.id
  ip_address    = hcloud_server.janeway.ipv4_address
  dns_ptr       = local.mail_fqdn
}

resource "hcloud_rdns" "janeway_ipv6" {
  primary_ip_id = hcloud_primary_ip.janeway_ipv6.id
  ip_address    = hcloud_server.janeway.ipv6_address
  dns_ptr       = local.mail_fqdn
}
