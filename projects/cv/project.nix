{ getSystem, ... }:
{
  gitignore =
    [
      "*.aux"
      "*.log"
      "*.out"
      "*.pdf"
    ]
    |> builtins.map (path: "projects/cv/**/${path}");

  perSystem =
    {
      pkgs,
      config,
      lib,
      ...
    }:
    {
      make-shells.cv = {
        inputsFrom = [
          config.make-shells.default.finalPackage
        ];
        buildInputs = with pkgs; [
          texlive.combined.scheme-full
        ];
      };
      packages = {
        cv =
          pkgs.runCommandNoCC "cv"
            {
              FONTCONFIG_FILE = pkgs.writeText "fonts.conf" ''
                <?xml version="1.0"?>
                <!DOCTYPE fontconfig SYSTEM "fonts.dtd">
                <fontconfig>
                  <cachedir>/tmp/fontconfig-cache</cachedir>
                </fontconfig>
              '';
            }
            ''
              mkdir -p /tmp/fontconfig-cache
              cp -r ${./src}/* .
              ${lib.getExe' pkgs.texlive.combined.scheme-full "xelatex"} cv.tex
              mkdir $out
              cp {cv.pdf,index.html} $out
            '';
      };
      treefmt.programs = {
        texfmt = {
          enable = true;
          includes =
            [
              "tex"
              "sty"
              "cls"
              "bib"
              "cmh"
            ]
            |> lib.map (ext: "projects/cv/**/*.${ext}");
        };
      };
    };

  configurations.nixos.sisko.module =
    { pkgs, ... }:
    {
      services.nginx.virtualHosts."cv.aciceri.dev" = {
        forceSSL = true;
        enableACME = true;
        locations."/".root = (getSystem pkgs.stdenv.system).packages.cv;
      };
    };
}
