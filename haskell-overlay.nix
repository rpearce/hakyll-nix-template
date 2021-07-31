final: prev:
  let
    inherit (prev.stdenv) mkDerivation;
    inherit (prev.lib.trivial) flip pipe;
    inherit (prev.haskell.lib)
      appendPatch
      appendConfigureFlags
      dontCheck
      doJailbreak;

    withPatch = flip appendPatch;
    withFlags = flip appendConfigureFlags;

    haskellCompiler = "ghc884";
  in {
    myHaskellPackages = prev.haskell.packages.${haskellCompiler}.override {
      overrides = hpFinal: hpPrev:
        let
          hakyll-src = hpPrev.callHackage "hakyll" "4.14.0.0" {};
          pandoc-src = hpPrev.callHackage "pandoc" "2.11.4" {}; # version specified by hayll 4.14.0.0
          slugger-src = hpPrev.callHackageDirect {
            pkg = "slugger";
            ver = "0.1.0.1";
            sha256 = "sha256-ggeo5TcbI4UlK/CtG4878USX9Cm7Faz16phdjlDOGaI=";
          } {}; # not available yet because it's so new
        in rec {
          hakyll = pipe hakyll-src [
            doJailbreak
            dontCheck
            (withPatch ./hakyll.patch)
            (withFlags [ "-f" "watchServer" "-f" "previewServer" ])
          ];

          pandoc = pipe pandoc-src [
            doJailbreak
            dontCheck
          ];

          slugger = slugger-src;

          ssg = hpPrev.callCabal2nix "ssg" ./ssg {};

          website = prev.stdenv.mkDerivation {
            #__contentAddressed = true; # uncomment if using cas: https://www.tweag.io/blog/2020-09-10-nix-cas/
            name = "website";
            buildInputs = [ ssg ];
            src = prev.nix-gitignore.gitignoreSourcePure [
              ./.gitignore
              ".git"
              ".github"
            ] ./.;

            # LANG and LOCALE_ARCHIVE are fixes pulled from the community:
            #   https://github.com/jaspervdj/hakyll/issues/614#issuecomment-411520691
            #   https://github.com/NixOS/nix/issues/318#issuecomment-52986702
            #   https://github.com/MaxDaten/brutal-recipes/blob/source/default.nix#L24
            LANG = "en_US.UTF-8";
            LOCALE_ARCHIVE = prev.lib.optionalString
              (prev.buildPlatform.libc == "glibc")
              "${prev.glibcLocales}/lib/locale/locale-archive";

            buildPhase = ''
              hakyll-site build --verbose
            '';

            installPhase = ''
              mkdir -p "$out/dist"
              cp -r dist/* "$out/dist"
            '';
          };
        };
    };
}
