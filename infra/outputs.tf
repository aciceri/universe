output "janeway_ipv4" {
  description = "Target of `nixos-anywhere --target-host root@…` for the first install."
  value       = hcloud_server.janeway.ipv4_address
}

output "janeway_ipv6" {
  value = hcloud_server.janeway.ipv6_address
}


output "janeway_backup_account" {
  description = "SFTP login of the Storage Box subaccount janeway's restic repository lives in."
  value       = "${hcloud_storage_box_subaccount.janeway.username}@${hcloud_storage_box_subaccount.janeway.server}"
}
output "zone_name_servers" {
  description = "Sanity check that ciceri.me is really served by Cloudflare."
  value       = data.cloudflare_zone.root.name_servers
}
