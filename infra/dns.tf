locals {
  domain = "ciceri.me"

  # SSH/deploy name of the machine.
  host_fqdn = "janeway.${local.domain}"

  # Name the mail server announces in EHLO and the target of the MX record. It
  # is a separate name from the host on purpose: MX records must not point at a
  # CNAME, and the mail identity should survive moving to another machine.
  mail_fqdn = "mail.${local.domain}"

  # Roundcube.
  webmail_fqdn = "webmail.${local.domain}"

  dkim_selector = "mail"
}

# ciceri.me is registered with Cloudflare Registrar, so the zone already exists
# and is not managed here: OpenTofu only owns the records below.
data "cloudflare_zone" "root" {
  filter = {
    name = local.domain
  }
}

# Records are DNS-only (grey cloud): the proxy only handles HTTP, and an
# orange-clouded name would publish Cloudflare's addresses to the SMTP world.
resource "cloudflare_dns_record" "janeway_a" {
  zone_id = data.cloudflare_zone.root.id
  name    = local.host_fqdn
  type    = "A"
  content = hcloud_server.janeway.ipv4_address
  ttl     = 300
  proxied = false
  comment = "janeway, managed in infra/"
}

resource "cloudflare_dns_record" "janeway_aaaa" {
  zone_id = data.cloudflare_zone.root.id
  name    = local.host_fqdn
  type    = "AAAA"
  content = hcloud_server.janeway.ipv6_address
  ttl     = 300
  proxied = false
  comment = "janeway, managed in infra/"
}

resource "cloudflare_dns_record" "mail_a" {
  zone_id = data.cloudflare_zone.root.id
  name    = local.mail_fqdn
  type    = "A"
  content = hcloud_server.janeway.ipv4_address
  ttl     = 300
  proxied = false
  comment = "mailserver fqdn, managed in infra/"
}

resource "cloudflare_dns_record" "mail_aaaa" {
  zone_id = data.cloudflare_zone.root.id
  name    = local.mail_fqdn
  type    = "AAAA"
  content = hcloud_server.janeway.ipv6_address
  ttl     = 300
  proxied = false
  comment = "mailserver fqdn, managed in infra/"
}

# Roundcube. Left grey-clouded like the rest: proxying it would only hide the
# origin from a service that already terminates its own Let's Encrypt cert.
resource "cloudflare_dns_record" "webmail_a" {
  zone_id = data.cloudflare_zone.root.id
  name    = local.webmail_fqdn
  type    = "A"
  content = hcloud_server.janeway.ipv4_address
  ttl     = 300
  proxied = false
  comment = "roundcube, managed in infra/"
}

resource "cloudflare_dns_record" "webmail_aaaa" {
  zone_id = data.cloudflare_zone.root.id
  name    = local.webmail_fqdn
  type    = "AAAA"
  content = hcloud_server.janeway.ipv6_address
  ttl     = 300
  proxied = false
  comment = "roundcube, managed in infra/"
}

resource "cloudflare_dns_record" "mx" {
  zone_id  = data.cloudflare_zone.root.id
  name     = local.domain
  type     = "MX"
  content  = local.mail_fqdn
  priority = 10
  ttl      = 3600
  comment  = "managed in infra/"
}

# `mx` authorizes exactly the hosts listed in the MX records above, so the
# policy follows the mail server automatically. `-all` hard-fails everything
# else: nothing but janeway ever sends as @ciceri.me.
resource "cloudflare_dns_record" "spf" {
  zone_id = data.cloudflare_zone.root.id
  name    = local.domain
  type    = "TXT"
  content = "v=spf1 mx -all"
  ttl     = 3600
  comment = "SPF, managed in infra/"
}

# Strict alignment: both SPF and DKIM must match the From: domain exactly.
# `reject` is safe here because janeway is the only thing that will ever send
# as @ciceri.me, and both SPF and DKIM are published before any mail leaves.
resource "cloudflare_dns_record" "dmarc" {
  zone_id = data.cloudflare_zone.root.id
  name    = "_dmarc.${local.domain}"
  type    = "TXT"
  content = "v=DMARC1; p=reject; adkim=s; aspf=s; rua=mailto:postmaster@${local.domain}"
  ttl     = 3600
  comment = "DMARC, managed in infra/"
}

# rspamd generates the key pair on janeway itself, so this record can only be
# filled in after the first boot; see the DKIM section of the README.
resource "cloudflare_dns_record" "dkim" {
  count = var.janeway_dkim_public_key == "" ? 0 : 1

  zone_id = data.cloudflare_zone.root.id
  name    = "${local.dkim_selector}._domainkey.${local.domain}"
  type    = "TXT"
  content = "v=DKIM1; k=rsa; p=${var.janeway_dkim_public_key}"
  ttl     = 3600
  comment = "DKIM, managed in infra/"
}

# RFC 6186 autodiscovery, so clients find the right ports without being told.
# Only the implicit-TLS services exist: the STARTTLS ones are off by default in
# simple-nixos-mailserver and stay off.
resource "cloudflare_dns_record" "srv_submissions" {
  zone_id = data.cloudflare_zone.root.id
  name    = "_submissions._tcp.${local.domain}"
  type    = "SRV"
  ttl     = 3600
  comment = "managed in infra/"

  # The API echoes the priority back at the top level as well as inside `data`,
  # so both have to be set or every plan reports a phantom change.
  priority = 10

  data = {
    priority = 10
    weight   = 1
    port     = 465
    target   = local.mail_fqdn
  }
}

resource "cloudflare_dns_record" "srv_imaps" {
  zone_id  = data.cloudflare_zone.root.id
  name     = "_imaps._tcp.${local.domain}"
  type     = "SRV"
  ttl      = 3600
  comment  = "managed in infra/"
  priority = 10

  data = {
    priority = 10
    weight   = 1
    port     = 993
    target   = local.mail_fqdn
  }
}
