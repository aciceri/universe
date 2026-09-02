{
  config,
  lib,
  getCurrentDir,
  ...
}:
let
  currentDir = getCurrentDir __curPos;
in
{
  # OpenTofu reads all three straight from the environment: agenix-shell exports
  # every secret's content under its uppercased name, and those names are
  # exactly what the providers (CLOUDFLARE_API_TOKEN, HCLOUD_TOKEN) look for.
  secrets = {
    cloudflare_api_token = { };
    hcloud_token = { };
    tofu_state_passphrase = { };
  };

  gitignore =
    [
      ".terraform"
      ".terraform.tfstate.lock.info"
      # The state itself is committed (encrypted); the backups are not. Two
      # shapes exist: `terraform.tfstate.backup` written on every apply, and
      # `terraform.tfstate.<epoch>.backup` written by `tofu import`.
      "*.tfstate.backup"
      "*.tfstate.*.backup"
      "*.tfplan"
      "crash.log"
      "crash.*.log"
    ]
    |> lib.map (pattern: "${currentDir}/${pattern}");

  perSystem =
    { pkgs, ... }:
    {
      make-shells.default = {
        packages = [ pkgs.opentofu ];

        # Secrets that OpenTofu consumes as input variables rather than as
        # provider credentials have to be renamed to the TF_VAR_ convention.
        # mkAfter: agenix-shell's own hook must have run first.
        shellHook = lib.mkAfter ''
          export TF_VAR_state_passphrase="''${TOFU_STATE_PASSPHRASE:-}"
          export TF_VAR_storage_box_password="''${HETZNER_STORAGE_BOX_SSH_PASSWORD:-}"
          export TF_VAR_storage_box_janeway_password="''${HETZNER_STORAGE_BOX_JANEWAY_PASSWORD:-}"
        '';
      };

      # The SSH keys authorized on a freshly created server come from the flake,
      # not from a hand-maintained list of strings inside the HCL.
      files.files = [
        {
          path_ = "${currentDir}/nix.auto.tfvars.json";
          drv = (pkgs.formats.json { }).generate "nix.auto.tfvars.json" {
            ssh_public_keys = config.users.ccr.sshKeys;
          };
        }
      ];

      treefmt.programs.terraform = {
        enable = true;
        includes = [ "${currentDir}/*.tf" ];
      };
    };

  readme.parts.infra = ''
    ## Infrastructure

    [`infra/`](infra/) is an [OpenTofu](https://opentofu.org/) project owning the
    parts of the fleet that cannot be expressed as a NixOS configuration: the
    Hetzner Cloud machine [Janeway](#janeway), the Storage Box the fleet backs
    up to, and DNS on Cloudflare.

    `ciceri.me` is managed here in full. `aciceri.dev` is **shared**: every
    static record in it — the registrar's MX set, the mesh names, the DNSLink
    and verification tokens — is declared in
    [`dns-aciceri.tf`](infra/dns-aciceri.tf), while the names that point at
    home stay with [`cloudflare-dyndns`](modules/cloudflare-dyndns.nix),
    because that address changes. OpenTofu only ever knows the records in its
    own state, so the two never see each other; the rule that keeps it that
    way is written at the top of that file.

    Everything the project needs comes from the dev shell: `tofu` itself, the
    API tokens, the Storage Box passwords and the passphrase encrypting the
    state, all decrypted by
    [agenix-shell](https://github.com/aciceri/agenix-shell). The state and plan
    files are committed to this repository, encrypted client side.

    ```bash
    cd infra
    tofu init
    tofu plan
    tofu apply
    ```

    ### Installing janeway

    OpenTofu only creates the machine and leaves it on Hetzner's stock Debian;
    NixOS is put on it by nixos-anywhere. The host key has to travel along,
    otherwise agenix cannot decrypt anything on the first boot:

    ```bash
    extra=$(mktemp -d)
    install -d -m 755 "$extra/etc/ssh"
    (umask 077; rage -d -i ~/.ssh/id_ed25519 \
      -o "$extra/etc/ssh/ssh_host_ed25519_key" secrets/ssh_host_key_janeway.age)

    nix run nixpkgs#nixos-anywhere -- \
      --flake .#janeway \
      --extra-files "$extra" \
      --generate-hardware-config nixos-facter hosts/janeway/facter.json \
      --target-host "root@$(tofu -chdir=infra output -raw janeway_ipv4)"

    rm -rf "$extra"
    ```

    Commit the generated `hosts/janeway/facter.json`. From then on the machine
    is deployed like any other, over the mesh, either from the `Deploy`
    workflow or with `nixos-rebuild switch --flake .#janeway --target-host
    root@janeway.wg.aciceri.dev`.

    ### DKIM

    rspamd generates the key pair on the machine itself, so the public half can
    only be published after the first boot. It lives in
    [`dkim.auto.tfvars`](infra/dkim.auto.tfvars); to rotate it, read the new key
    back, glue the quoted chunks into a single string and re-apply:

    ```bash
    ssh root@janeway.wg.aciceri.dev cat /var/dkim/ciceri.me.mail.txt
    echo 'janeway_dkim_public_key = "MIIBIjANBg..."' > infra/dkim.auto.tfvars
    tofu -chdir=infra apply
    ```

    ### Outbound SMTP

    Hetzner blocks outbound 25/465/587 on *new* projects; on this one only
    465 and 587 are blocked, and janeway needs neither since it delivers
    directly on 25. If that ever changes, the unblock is requested from the
    Cloud Console (the project's limits/support form), not from Robot.

    The whole chain is verifiable from the outside by mailing
    `check-auth@verifier.port25.com` from the account and reading the reply:
    SPF, DKIM and `iprev` must all come back `pass`.
  '';
}
