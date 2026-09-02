# aciceri.dev is a shared zone: the names that point at home are rewritten by
# cloudflare-dyndns whenever the domestic address changes, and OpenTofu must
# never touch those. Everything else in the zone is static, so it lives here.
#
# OpenTofu has no notion of "owning a zone": state contains the records listed
# below and a plan never proposes anything about a record it does not know, so
# the two managers coexist without seeing each other. The rule that keeps it
# that way, and the only one that matters: **never declare here a name that
# appears in services.cloudflare-dyndns.domains** (modules/cloudflare-dyndns.nix).
# Both would rewrite it, forever, in turn.
#
# So the only records of the zone missing from the map below are the dyndns
# ones. `matrix.aciceri.dev` and `vault.aciceri.dev` used to be a third
# category — A records aimed at the domestic address that nothing kept up to
# date, pointing at names no nginx ever served — and were deleted rather than
# frozen into code.
data "cloudflare_zone" "aciceri" {
  filter = {
    name = "aciceri.dev"
  }
}

locals {
  # Keyed by "<type> <name>", with the mail server appended for the MX set,
  # since five records share one name and type.
  aciceri_records = {

    # Email forwarding at the registrar: the MX set plus the SPF record that
    # authorises its relays. aciceri.dev does not host its own mail.
    "MX aciceri.dev eforward1" = { name = "aciceri.dev", type = "MX", content = "eforward1.registrar-servers.com", priority = 10, ttl = 1, proxied = false }
    "MX aciceri.dev eforward2" = { name = "aciceri.dev", type = "MX", content = "eforward2.registrar-servers.com", priority = 10, ttl = 1, proxied = false }
    "MX aciceri.dev eforward3" = { name = "aciceri.dev", type = "MX", content = "eforward3.registrar-servers.com", priority = 10, ttl = 1, proxied = false }
    "MX aciceri.dev eforward4" = { name = "aciceri.dev", type = "MX", content = "eforward4.registrar-servers.com", priority = 15, ttl = 1, proxied = false }
    "MX aciceri.dev eforward5" = { name = "aciceri.dev", type = "MX", content = "eforward5.registrar-servers.com", priority = 20, ttl = 1, proxied = false }
    "TXT aciceri.dev"          = { name = "aciceri.dev", type = "TXT", content = "v=spf1 include:spf.efwd.registrar-servers.com ~all", ttl = 1, proxied = false }

    # WireGuard mesh, one name plus a wildcard per host. Mirrors the
    # addressing plan in modules/wireguard.nix.
    "A *.janeway.wg.aciceri.dev"   = { name = "*.janeway.wg.aciceri.dev", type = "A", content = "10.100.0.9", ttl = 1, proxied = false, comment = "janeway on the mesh, managed in infra/" }
    "A *.kirk.wg.aciceri.dev"      = { name = "*.kirk.wg.aciceri.dev", type = "A", content = "10.100.0.3", ttl = 1, proxied = false }
    "A *.oneplus8t.wg.aciceri.dev" = { name = "*.oneplus8t.wg.aciceri.dev", type = "A", content = "10.100.0.4", ttl = 1, proxied = false }
    "A *.picard.wg.aciceri.dev"    = { name = "*.picard.wg.aciceri.dev", type = "A", content = "10.100.0.2", ttl = 1, proxied = false }
    "A *.pike.wg.aciceri.dev"      = { name = "*.pike.wg.aciceri.dev", type = "A", content = "10.100.0.8", ttl = 1, proxied = false }
    "A *.sisko.wg.aciceri.dev"     = { name = "*.sisko.wg.aciceri.dev", type = "A", content = "10.100.0.1", ttl = 1, proxied = false }
    "A janeway.wg.aciceri.dev"     = { name = "janeway.wg.aciceri.dev", type = "A", content = "10.100.0.9", ttl = 1, proxied = false, comment = "janeway on the mesh, managed in infra/" }
    "A kirk.wg.aciceri.dev"        = { name = "kirk.wg.aciceri.dev", type = "A", content = "10.100.0.3", ttl = 1, proxied = false }
    "A oneplus8t.wg.aciceri.dev"   = { name = "oneplus8t.wg.aciceri.dev", type = "A", content = "10.100.0.4", ttl = 1, proxied = false }
    "A picard.wg.aciceri.dev"      = { name = "picard.wg.aciceri.dev", type = "A", content = "10.100.0.2", ttl = 1, proxied = false }
    "A pike.wg.aciceri.dev"        = { name = "pike.wg.aciceri.dev", type = "A", content = "10.100.0.8", ttl = 1, proxied = false }
    "A sisko.wg.aciceri.dev"       = { name = "sisko.wg.aciceri.dev", type = "A", content = "10.100.0.1", ttl = 1, proxied = false }

    # The older ZeroTier network. Still resolvable, no longer used by
    # anything the flake configures.
    "A *.kirk.zt.aciceri.dev"      = { name = "*.kirk.zt.aciceri.dev", type = "A", content = "10.100.1.3", ttl = 1, proxied = false }
    "A *.oneplus8t.zt.aciceri.dev" = { name = "*.oneplus8t.zt.aciceri.dev", type = "A", content = "10.100.1.4", ttl = 1, proxied = false }
    "A *.picard.zt.aciceri.dev"    = { name = "*.picard.zt.aciceri.dev", type = "A", content = "10.100.1.2", ttl = 1, proxied = false }
    "A *.pike.zt.aciceri.dev"      = { name = "*.pike.zt.aciceri.dev", type = "A", content = "10.100.1.6", ttl = 1, proxied = false }
    "A *.sisko.zt.aciceri.dev"     = { name = "*.sisko.zt.aciceri.dev", type = "A", content = "10.100.1.1", ttl = 1, proxied = false }
    "A kirk.zt.aciceri.dev"        = { name = "kirk.zt.aciceri.dev", type = "A", content = "10.100.1.3", ttl = 1, proxied = false }
    "A oneplus8t.zt.aciceri.dev"   = { name = "oneplus8t.zt.aciceri.dev", type = "A", content = "10.100.1.4", ttl = 1, proxied = false }
    "A picard.zt.aciceri.dev"      = { name = "picard.zt.aciceri.dev", type = "A", content = "10.100.1.2", ttl = 1, proxied = false }
    "A sisko.zt.aciceri.dev"       = { name = "sisko.zt.aciceri.dev", type = "A", content = "10.100.1.1", ttl = 1, proxied = false }

    # IPFS DNSLink records for the blog and the CV.
    "TXT _dnslink.aciceri.dev"      = { name = "_dnslink.aciceri.dev", type = "TXT", content = "dnslink=/ipfs/QmPH2Y2mTiLY4QCnSPLJEgdoi96SuFpqMitHqfiWbTAdi3", ttl = 1, proxied = false }
    "TXT _dnslink.blog.aciceri.dev" = { name = "_dnslink.blog.aciceri.dev", type = "TXT", content = "dnslink=/ipfs/QmVkVk6suc6y6C6Gu1Leh14MKhoMBoU7NDNyjVYtX3sLSs", ttl = 1, proxied = false }
    "TXT _dnslink.cv.aciceri.dev"   = { name = "_dnslink.cv.aciceri.dev", type = "TXT", content = "dnslink=/ipfs/QmUDNDpfQVZ4etr6ztQnKYTVb1w1RGxqXCFTRKSyVmNuuM", ttl = 1, proxied = false }

    # Third-party verification tokens.
    "TXT ideacorp.aciceri.dev" = { name = "ideacorp.aciceri.dev", type = "TXT", content = "google-site-verification=3uJvRWB7iIXTYZGZbrtqkMGlP9bg1AaZmMHHFyu1eAE", ttl = 1, proxied = false }

    # Everything else that is static.
    "A *.nix-pizza.aciceri.dev" = { name = "*.nix-pizza.aciceri.dev", type = "A", content = "49.13.120.184", ttl = 1, proxied = false }
    "CNAME www.aciceri.dev"     = { name = "www.aciceri.dev", type = "CNAME", content = "aciceri.dev", ttl = 1, proxied = true }
  }
}

resource "cloudflare_dns_record" "aciceri" {
  for_each = local.aciceri_records

  zone_id  = data.cloudflare_zone.aciceri.id
  name     = each.value.name
  type     = each.value.type
  content  = each.value.content
  ttl      = each.value.ttl
  proxied  = each.value.proxied
  priority = try(each.value.priority, null)
  comment  = try(each.value.comment, null)
}
