# Both providers read their credentials from the environment, where agenix-shell
# decrypts them: CLOUDFLARE_API_TOKEN and HCLOUD_TOKEN. Nothing to configure.
provider "cloudflare" {}

provider "hcloud" {}
