{ compiler ? "ghc884"
, pkgs
}:
let
  inherit (pkgs.lib.trivial) flip pipe;
  inherit (pkgs.haskell.lib) appendPatch appendConfigureFlags dontCheck;

  hakyllFlags = [ "-f" "watchServer" "-f" "previewServer" ];

  haskellPackages = pkgs.haskell.packages.${compiler}.override {
    overrides = hpNew: hpOld: {
      hakyll =
        pipe
          hpOld.hakyll
          [
            (flip appendPatch ./hakyll.patch)
            (flip appendConfigureFlags hakyllFlags)
          ];

      hakyll-nix-template = hpNew.callCabal2nix "hakyll-nix-template" ./. { };

      # when hakyll is marked as broken in nixpkgs
      # because of version issues, fix them here:

      hslua = dontCheck (hpNew.callHackage "hslua" "1.0.3.2" { });
      jira-wiki-markup = dontCheck (hpNew.callHackage "jira-wiki-markup" "1.1.4" { });
      pandoc = dontCheck (hpNew.callHackage "pandoc" "2.9.2.1" { });
      pandoc-types = dontCheck (hpNew.callHackage "pandoc-types" "1.20" { });
    };
  };
in
haskellPackages
