# nix build -f _test.nix -L --rebuild
{
  pkgs ? import <nixpkgs> { },
}:
pkgs.testers.nixosTest {
  name = "forgejo-test";

  nodes.server = {
    services.forgejo = {
      enable = true;
      settings = {
        server = {
          HTTP_PORT = 3000;
          DOMAIN = "localhost";
        };
        service.DISABLE_REGISTRATION = false;
      };
      # Crea un utente iniziale per il test
      database.type = "sqlite3";
    };

    environment.systemPackages = with pkgs; [
      git
      curl
      jq
      forgejo
    ];
  };

  testScript = ''
    server.start()
    server.wait_for_unit("forgejo.service")
    server.wait_for_open_port(3000)

    # Verifica che Forgejo risponda
    server.succeed("curl --fail http://localhost:3000/")
    print("✓ Forgejo is running!")

    # Crea utente usando il CLI con il config file esplicito
    server.succeed(
        "sudo -u forgejo forgejo --config /var/lib/forgejo/custom/conf/app.ini "
        "admin user create --admin --username testuser --password password123 --email test@example.com"
    )
    print("✓ User created")

    # Genera token API
    token = server.succeed(
        "sudo -u forgejo forgejo --config /var/lib/forgejo/custom/conf/app.ini "
        "admin user generate-access-token -u testuser --scopes write:repository,write:user --raw"
    ).strip()
    print(f"✓ API token: {token[:8]}...")

    # Crea un repository via API
    server.succeed(
        f"curl --fail -X POST http://localhost:3000/api/v1/user/repos "
        f"-H 'Authorization: token {token}' "
        f"-H 'Content-Type: application/json' "
        f"-d '{{\"name\":\"test-repo\", \"private\":false}}'"
    )
    print("✓ Repository created via API")

    # Configura git e crea un commit
    server.succeed("git config --global user.email 'test@example.com'")
    server.succeed("git config --global user.name 'Test User'")
    server.succeed("mkdir -p /tmp/repo && cd /tmp/repo && git init")
    server.succeed("echo 'Hello from NixOS test!' > /tmp/repo/README.md")
    server.succeed("cd /tmp/repo && git add README.md && git commit -m 'Initial commit'")
    print("✓ Local git repository created")

    # Push al repository Forgejo
    server.succeed(
        f"cd /tmp/repo && git remote add origin http://testuser:{token}@localhost:3000/testuser/test-repo.git"
    )
    server.succeed("cd /tmp/repo && git push -u origin master")
    print("✓ Pushed to Forgejo repository")

    # Verifica che il repository sia accessibile via API
    server.succeed(
        f"curl --fail http://localhost:3000/api/v1/repos/testuser/test-repo "
        f"-H 'Authorization: token {token}'"
    )
    print("✓ Repository verified via API!")

    # Clone del repository per verificare l'intero ciclo
    server.succeed(
        f"git clone http://testuser:{token}@localhost:3000/testuser/test-repo.git /tmp/cloned-repo"
    )
    print("✓ Repository cloned successfully!")

    # Verifica che il file sia presente nel clone
    content = server.succeed("cat /tmp/cloned-repo/README.md")
    assert "Hello from NixOS test!" in content
    print("✓ File content verified in cloned repo!")

    print("\n🎉 Test completato con successo!")
  '';
}
