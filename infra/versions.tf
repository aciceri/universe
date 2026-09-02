terraform {
  required_version = ">= 1.12"

  required_providers {
    cloudflare = {
      source  = "cloudflare/cloudflare"
      version = "~> 5.24"
    }
    hcloud = {
      source  = "hetznercloud/hcloud"
      version = "~> 1.68"
    }
  }

  # State and plan files live in this repository, so they are encrypted client
  # side. The passphrase is an input variable rather than a literal: agenix-shell
  # exports it as TF_VAR_state_passphrase, and `enforced` makes every command
  # fail closed when it is missing instead of silently writing plaintext.
  #
  # Renaming the key provider or the method breaks decryption of the existing
  # state: their names are stored in the state metadata.
  encryption {
    key_provider "pbkdf2" "state" {
      passphrase = var.state_passphrase
    }

    method "aes_gcm" "state" {
      keys = key_provider.pbkdf2.state
    }

    state {
      method   = method.aes_gcm.state
      enforced = true
    }

    plan {
      method   = method.aes_gcm.state
      enforced = true
    }
  }
}
