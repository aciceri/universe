{ getSystem, getCurrentDir, ... }:
let
  currentDir = getCurrentDir __curPos;
in
{
  gitignore =
    [
      "*.html"
    ]
    |> builtins.map (path: "${currentDir}/**/${path}");

  perSystem =
    {
      pkgs,
      config,
      lib,
      ...
    }:
    let
      reveal-js = pkgs.fetchFromGitHub {
        owner = "hakimel";
        repo = "reveal.js";
        rev = "4.6.0";
        hash = "sha256-a+J+GasFmRvu5cJ1GLXscoJ+owzFXsLhCbeDbYChkyQ=";
      };

      emacs = (pkgs.emacsPackagesFor pkgs.emacs).emacsWithPackages (epkgs: [ epkgs.org-re-reveal ]);

      emacsExportScript = pkgs.writeScriptBin "emacs-export.el" ''
        #!${lib.getExe emacs} --script
        (require 'org-re-reveal)
        (switch-to-buffer (find-file (car argv)))
        (org-re-reveal-export-to-html)
      '';

      serve =
        pkgs.writers.writePython3 "serve.py"
          {
            flakeIgnore = [ "E501" ];
          }
          ''
            from http.server import HTTPServer, SimpleHTTPRequestHandler


            class Handler(SimpleHTTPRequestHandler):
                def do_GET(self):
                    if self.path.startswith('/reveal.js/plugin/'):
                        self.directory = '${config.packages.reveal-js}/plugin/'
                        self.path = self.path.replace('/reveal.js/plugin/', "")
                        return SimpleHTTPRequestHandler.do_GET(self)
                    elif self.path.startswith('/reveal.js/dist/'):
                        self.directory = '${config.packages.reveal-js}/dist/'
                        self.path = self.path.replace('/reveal.js/dist/', "")
                        return SimpleHTTPRequestHandler.do_GET(self)
                    else:
                        self.send_response(200)
                        self.end_headers()
                        with open('talk.html', 'rb') as f:
                            self.copyfile(f, self.wfile)


            server = HTTPServer(("", 8080), Handler)
            server.serve_forever()
          '';
    in
    {
      packages.nixos-devops-talk =
        pkgs.runCommandNoCC "nixos-devops-talk"
          {
            buildInputs = [ emacsExportScript ];
            passthru.serve = { inherit serve; };
          }
          ''
            cp ${./talk.org} talk.org
            emacs-export.el talk.org
            mkdir -p $out/reveal.js
            cp -r ${reveal-js}/{plugin,dist} $out/reveal.js/
            mv talk.html $out/index.html
            cp -r ${./pics} $out/pics
          '';
    };

  configurations.nixos.sisko.module =
    { pkgs, ... }:
    {
      services.nginx.virtualHosts."nixos-devops.talks.aciceri.dev" = {
        forceSSL = true;
        enableACME = true;
        locations."/".root = (getSystem pkgs.stdenv.system).packages.nixos-devops-talk;
      };
    };

  readme.parts.projects = ''
    ### "NixOS per DevOps" talk

    Talk introducing NixOS for DevOps, in italian.
    [Here](https://www.youtube.com/watch?v=dH3_H2ixvzg) the recording.
  '';
}
