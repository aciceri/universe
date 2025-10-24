# NixOS testing framework

**Ovvero come testare applicativi, anche complessi, in maniera completamente riproducibile**

<br>

_Un'introduzione gentile e pratica_

Note:
RICORDATI DI SPEGNERE OGNI COSA POSSA PRODURRE NOTIFICHE!

Il talk sarà diviso in tre parti:

- prima darò una introduzione velocissima su cosa sono Nix e NixOS
- poi parlerò dei NixOS test e perché mi piacciono
- infine scriveremo assieme un NixOS test partendo da zero
  Il talk è pensato anche per chi non ha mai visto Nix con l'obiettivo di incuriosirlo,
  ovviamente moltissimi concetti andranno accettati a scatola chiusa e un po' di intuizione
  sarà necessaria.

---

## Brevi cenni su Nix

<div class="fragment">

**2003** - Eelco Dolstra inizia il progetto durante il suo dottorato all'Università di Utrecht

</div>

<div class="fragment">

**2006** - Tesi di dottorato: _"The Purely Functional Software Deployment Model"_

</div>

<div class="fragment">

**Idee chiave:**

</div>

<div class="fragment">

- Build puramente funzionali

</div>

<div class="fragment">

- Riproducibilità

</div>

<div class="fragment">

- Isolamento completo delle dipendenze

</div>

<div class="fragment">

- Store immutabile (`/nix/store`)

</div>

Note:
Nix nasce nel 2003 come progetto di ricerca di Eelco Dolstra.
La sua tesi di dottorato del 2006 introduce il concetto di "purely functional software deployment model".
Le idee chiave sono: build puramente funzionali, dove lo stesso input produce sempre lo stesso output,
la riproducibilità completa, l'isolamento delle dipendenze,
e uno store immutabile dove ogni package ha il suo hash unico.
Questi concetti sono alla base di tutto l'ecosistema Nix, incluso il testing framework che vedremo oggi.

---

### Cos'è Nix?

<div class="fragment">

#### 🗣️ Un linguaggio

Linguaggio funzionale puro, lazy, dinamicamente tipizzato per descrivere build e configurazioni

</div>

<div class="fragment">

#### 📦 Un package manager

Gestisce pacchetti in modo dichiarativo e riproducibile

</div>

<div class="fragment">

#### 🔨 Un build system

Garantisce riproducibilità tramite sandboxing

</div>

Note:
Quindi cos'è Nix esattamente?
È tre cose allo stesso tempo.
Prima di tutto è un linguaggio di programmazione: funzionale puro, lazy, dinamicamente tipizzato.
Lo usiamo per descrivere come buildare pacchetti e configurare sistemi.
È anche un package manager: gestisce le dipendenze in modo dichiarativo, può avere più versioni dello stesso pacchetto installate senza conflitti (risolve dependency hell)
E infine è un build system: ogni build è puramente funzionale, lo stesso input produce sempre lo stesso output, garantendo riproducibilità totale.

---

```nix
{pkgs ? import <nixpkgs> {}}: pkgs.stdenv.mkDerivation rec {
  pname = "test";
  version = "1.0.0";

  src = pkgs.fetchFromGitHub {
    owner = "foo";
    repo = "bar";
    rev = "v${version}";
    hash = "sha256-xxx";
  };

  nativeBuildInputs = with pkgs; [ gcc gnumake ];

  buildPhase = ''
    make build
  '';

  installPhase = ''
    install -Dm755 hello $out/bin/hello
  '';

  meta = with pkgs.lib; {
    description = "Derivazione di esempio";
    license = licenses.mit;
  };
}
```

<!-- .element: class="stretch code-tall" -->

Note:

- nix a prima vista sembra tipo un json con le funzioni
- per quanto piccolo come linguaggio è molto potente, forse anche troppo
- questa stessa presentazione è una derivazione il cui output è la cartella che servo con nginx (se vi interessa è tutto open source)

---

### Come avviene una build

<ul>
  <li class="fragment">
    l'interprete <code>nix</code> valuta e ritorna una derivazione (come “oggetto” in memoria)
  </li>
  <li class="fragment">
    la derivazione viene trasformata (“istanziata”) in un file <code>.drv</code> che finisce nel cosiddetto store immutabile (<code>/nix/store</code>)
  </li>
  <li class="fragment">
    …assieme ad altri file <code>.drv</code> rappresentanti tutte le dipendenze (grafo)
  </li>
  <li class="fragment">
    il “builder” <code>nix</code> (solitamente un demone) builda il grafo e per ogni derivazione ne infila l’output nello store
  </li>
</ul>

Note:

- naming di merda, sia la parola `nix` che `derivation` sono estremamente sovraccariche
- notare che quella di prima in realtà valuta una funzione, ignoriamo come viene passato `pkgs` per semplicita
- abbiamo completamente ignorato il fatto che nix assegna un address ad ogni derivazione, che dipende dall'address delle derivazioni dipendenze.
- quando il builder builda il grafo ovviamente se quella specifica derivazione è gia' stata buildata (stesso address) non viene buildata di nuovo
- solitamente nix non builda tutto da zero ma interroga delle cache che sono trusted e nel caso scarica da li' i path che finiranno nello store

---

Grafo delle dipendenze di `glibc`

<img src="images/drv-graph.svg" />

Note:

- questo è il grafo a runtime di glibc
- ci sarebbe da parlare anche del grafo a buildtime ma non abbiamo tempo
- concetto di closure (chiusura transitiva) che rappresenta tutto cio' che ti serve per eseguire un software

---

#### E se una derivazione producesse in output l'intero sistema operativo?

<div>Ecco <b>NixOS</b></div> <!-- .element: class="fragment" -->

Note:

- è riproducibile!
- la sua closure sono tutti i software e le configurazioni di questi software (usa systemd)

---

#### `nixpkgs`

![nixpkgs](images/nixpkgs-screenshot.png)

<!-- .element: class="r-stretch" style="max-height: 80vh; object-fit: contain;" -->

Note:

- enorme repo con tutte le derivazioni di tutti i pacchetti (batte AUR alla grande)
- e tutti i NixOS module che ora vedremo

---

### NixOS module system

<ul>
  <li class="fragment">
    nel linguaggio Nix i tipi non posso essere dichiarati esplicitamente e non esiste un sistema di moduli integrato
  </li>
  <li class="fragment">
    è stato quindi creato un sistema di tipi e moduli nel linguaggio stesso
  </li>
  <li class="fragment">
    <code>nixpkgs</code> colleziona anche questi moduli che permettono di attivare e configurare ogni aspetto del sistema
  </li>
</ul>foo

Note:

- ecco cosa intendevo che forse nix è troppo piccolo come linguaggio ma al tempo stesso troppo potente, la gente ci costruisce sopra la qualunque in termini di astrazioni (questa pero' ha senso eh, lo giuro!)

---

```nix
{ config, ... }: {
  services.grafana = {
    enable = true;
    settings.server = {
      http_addr = "127.0.0.1";
      http_port = 3000;
      domain = "grafana.example.org";
    };
  };

  services.nginx = {
    enable = true;

    virtualHosts."grafana.example.org" = {
      forceSSL = true;
      enableACME = true;
      locations."/" = {
        proxyPass = "http://127.0.0.1:${toString config.services.grafana.settings.server.http_port}";
        proxyWebsockets = true;
      };
    };
  };

  networking.firewall.allowedTCPPorts = [ 80 443 ];
}
```

<!-- .element: class="stretch code-tall" -->

Note:

- creiamo systemd unit per grafana e nginx (per nginx creiamo anche timer per avere certificati ACME sempre validi)
- creiamo file di configurazione per grafana e nginx
- posso accedere al punto fisso della valutazione dei moduli tramite `config` (mostra la porta)
- apro il firewall
- https://search.nixos.org/options cerca grafana e fa vedere il sorgente

---

### NixOS testing framework

<ul>
  <li class="fragment">
    da una singola configurazione NixOS possiamo anche produrre derivazioni che buildano immagini di vario tipo
  </li>
  <li class="fragment">
    in particolare possiamo tirare fuori una immagine <b>QEMU</b>
  </li>
</ul>

<br>
<br>

<div class="fragment">
Dunque un <b>test NixOS</b> è una derivazione che builda una (o più) configurazioni NixOS e le esegue con QEMU
</div>

Note:

- finalmente ci siamo
- per esempio tiriamo fuori immagini rootfs da mettere su SD per sistemi embedded, immagini per provider cloud come AWS o Google Cloud Platform
- quindi tecnicamente un Nixos test è una derivazione che non produce niente ma dipende da derivazioni che buildano immagini QEMU. La build fallisce se il test non va a buon fine.

---

```nix
pkgs.nixosTest {
  name = "grafana-nginx-proxy";

  nodes.foo = { config, ... }:
    {
      # [...] qui attiviamo e configuriamo Grafana e Nginx come prima
      networking.firewall.allowedTCPPorts = [ 80 3000 ];
    };

  testScript = ''
    # la VM con la configurazione di `foo` è avviata in automatico
    foo.wait_for_unit("nginx.service")
    foo.wait_for_unit("grafana.service")

    foo.wait_for_open_port(80)
    foo.wait_for_open_port(3000)

    foo.succeed("curl --fail http://127.0.0.1:3000/login | grep 'Grafana'")
    foo.succeed(
      "curl --fail --header 'Host: grafana.example.test' http://127.0.0.1/ | grep 'Grafana'"
    )
  '';
}
```

<!-- .element: class="stretch code-tall" -->

Note:

- nixpkgs usa questi test per fare CI dei moduli
- sono derivazioni con loro closure, quindi il numero di build necessarie è sempre minimo, tutto viene cachato
- in teoria il cosidetto "driver" può essere scritto anche in altri linguaggi per ora solo python è implementato

---

# Grazie!

<div style="display: flex; align-items: center; justify-content: space-between; font-size: 0.8em;">
  <div style="flex: 1;">
    <img src="images/me.jpg" alt="Andrea Ciceri" style="border-radius: 50%; width: 8em; height: 8em; object-fit: cover;">
    <p><strong>Andrea Ciceri</strong><br>
    <a href="https://blog.aciceri.dev/contacts/" style="font-size: 0.8em; white-space: nowrap;">blog.aciceri.dev/contacts</a></p>
  </div>
  <div style="flex: 2; text-align: left; padding-left: 2rem;">
    <div style="display: flex; align-items: center; gap: 1em;">
      <div>
        <p><strong>Nix User Group Milano:</strong><br>
        <a href="https://milano.nix.pizza" style="white-space: nowrap;">milano.nix.pizza</a></p>
      </div>
      <canvas id="qr-canvas" style="width: 8em; height: 8em;"></canvas>
    </div>
    <p style="margin-top: 2rem;"><em>Grazie per l'attenzione!<br>
    Se avete altre domande fermatemi pure oggi durante l'evento o scrivetemi, sarò felicissimo di rispondere.</em></p>
  </div>
</div>

Note:
Per la fase di demo:foo

- https://nixos.org/manual/nixos/unstable/#sec-nixos-tests
- https://github.com/NixOS/nixpkgs/blob/master/nixos/tests/bittorrent.nix
- nix build nixpkgs#nixosTests.bittorrent -L --rebuild
- https://github.com/NixOS/nixpkgs/blob/master/nixos/tests/openarena.nix
- nix run nixpkgs#nixosTests.openarena.driverInteractive
- https://github.com/NixOS/nixpkgs/blob/master/nixos/tests/openarena.nix
- nix run nixpkgs#nixosTests.firefox.driverInteractive
- start_all()
- run_tests()
- imv ./result
