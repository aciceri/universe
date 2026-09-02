variable "state_passphrase" {
  description = "Passphrase encrypting the committed state and plan files, injected by agenix-shell as TF_VAR_state_passphrase."
  type        = string
  sensitive   = true

  validation {
    condition     = length(var.state_passphrase) >= 16
    error_message = "The pbkdf2 key provider rejects passphrases shorter than 16 characters."
  }
}

variable "ssh_public_keys" {
  description = "SSH public keys uploaded to Hetzner and authorized on freshly created servers. Generated from the flake into nix.auto.tfvars.json."
  type        = list(string)
}

variable "storage_box_password" {
  description = "Password of the pre-existing Storage Box, the same secret restic authenticates with. Injected by agenix-shell as TF_VAR_storage_box_password."
  type        = string
  sensitive   = true
}

variable "storage_box_janeway_password" {
  description = "Password of the Storage Box subaccount janeway backs up to. Injected by agenix-shell as TF_VAR_storage_box_janeway_password."
  type        = string
  sensitive   = true
}

variable "janeway_dkim_public_key" {
  description = <<-EOT
    Base64 body of the DKIM public key published at `mail._domainkey.ciceri.me`.
    Empty until janeway has booted once: rspamd generates the key pair on the
    machine, so read it back with

      ssh root@janeway.ciceri.me cat /var/dkim/ciceri.me.mail.txt

    glue the quoted chunks together and set this to everything after `p=`.
  EOT
  type        = string
  default     = ""
}
