Welcome to my blog, let $\Sigma$ be the set of the Unicode symbols, then this
site is a finite subset $\mathbb{B} \subset \Sigma$.
Assuming that your browser can parse and interpret $\mathbb{B}$, and hoping that
you find it more interesting than a generic subset, I wish you a pleasant
navigation.

The content of this blog is heterogeneous and I think that a better introduction
than this would be difficult, so let's move on.

![Me, in all my glory](images/me.jpg)
*Me, in all my glory*

It's also possible to reach this site in the [Tor network](https://www.torproject.org/) at this easy to
remember [onion link](http://ty7du6aabrwttfuh6hgvt4aowvmrqxscdshsrcjc2dzftewjs6qvsxad.onion), and in any case a snapshot with all the contents of the
latest commit is always downloadable [here](https://github.com/aciceri/test/archive/refs/heads/gh-pages.zip).
Since the website is meant to be self contained as much as possible the
experience you would have viewing this site on your local web server should be
almost the same.

## Who I am

I'm a math student who likes programming problems, I think this is a good and
concise definition compared to what this blog looks like.
However I'm more than this, I also appreciate a variety of different things like
cooking, [really nerd videogames](https://www.nethack.org), free software, music (light or classical, as a
listener or [as a player](../posts/midi-to-bach/)), sci-fi books, old and boring movies, etc...

Moreover, I'm not an english native speaker, so I ask you to be clement if you
find some errors, this site is also an opportunity to improve my language
skills.

## Under the hood

These pages are written in [GNU Emacs](https://www.gnu.org/software/emacs/) using the versatile [org-mode](https://orgmode.org/), the contents
are generated using [Hakyll](https://jaspervdj.be/hakyll/), a customizable static site generator written in
Haskell.
The website is meant to be lightweight and HTML5/CSS3 compliant,
however it should be perfecly readable also by older browser or even TUI browser
like the [Emacs web browser](https://www.gnu.org/software/emacs/manual/html_mono/eww.html) or [Lynx](https://lynx.browser.org/).
Some additional features, like the dark mode or the dynamically added hyphens,
may require the use of Javascript or cookies, but they are implemented in a
unobtrusive way, so the website is viewable even without javascript or cookies.

The design is minimal and completely realized by me, the fonts used
are [Latin Modern](https://en.wikipedia.org/wiki/Computer_Modern#Latin_Modern) and [Iosevka Comfy](https://github.com/protesilaos/iosevka-comfy). Furthermore, all the resources
like images, javascripts, stylesheets and fonts are hosted in the this
space, without using a CDN.

All the sources, both for the posts and the code, are inside the same [Github
repository](https://github.com/universe/tree/main/projects/blog).
The building, testing and deploying are managed by [Nix](https://nixos.org/nix/), using a [Flake](https://nixos.wiki/wiki/Flakes), locally on my machine
when I'm writing new posts and publically by a GitHub workflow when I push the commit to
the repository.
This elegant approach provides solid and replicable builds and a revision
control system.

Also, all of this is hosted using [GitHub Pages](https://pages.github.com/), and replicated on the [IPFS](https://ipfs.io/)
network, pinned on [Pinata](https://pinata.cloud/) and on my household node.
The last IPFS content hash is automatically updated using [DNSLink](https://dnslink.io/), so, if your
browser is correctly configured you should natively browse this website with
IPFS.
However you could also use a [public gateway](https://ipfs.github.io/public-gateway-checker/) with something like
<https://gateway.ipfs.io/ipns/blog.aciceri.dev/>
