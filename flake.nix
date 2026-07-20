{
  description = "hakyll-nix-template";

  nixConfig = {
    bash-prompt = "[hakyll-nix]λ ";
  };

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-26.05";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };

        # The Hakyll site generator, built from ./ssg against the Haskell
        # package set that ships with the pinned nixpkgs.
        #
        # NOTE: we deliberately avoid `haskell.lib.justStaticExecutables` here.
        # On aarch64-darwin the resulting binary still references GHC, which
        # trips that helper's disallowed-references check and fails the build.
        # The generated site is identical either way; this just keeps the build
        # reliable across Linux and macOS.
        hakyll-site = pkgs.haskellPackages.callPackage ./ssg { };

        website = pkgs.stdenv.mkDerivation {
          name = "website";
          src = pkgs.nix-gitignore.gitignoreSourcePure [
            ./.gitignore
            ".git"
            ".github"
          ] ./.;

          # LANG and LOCALE_ARCHIVE are fixes pulled from the community:
          #   https://github.com/jaspervdj/hakyll/issues/614#issuecomment-411520691
          #   https://github.com/NixOS/nix/issues/318#issuecomment-52986702
          #   https://github.com/MaxDaten/brutal-recipes/blob/source/default.nix#L24
          LANG = "en_US.UTF-8";
          LOCALE_ARCHIVE = pkgs.lib.optionalString
            (pkgs.stdenv.buildPlatform.libc == "glibc")
            "${pkgs.glibcLocales}/lib/locale/locale-archive";

          buildPhase = ''
            ${hakyll-site}/bin/hakyll-site build --verbose
          '';

          installPhase = ''
            mkdir -p "$out/dist"
            cp -a dist/. "$out/dist"
          '';
        };
      in
      {
        apps.default = flake-utils.lib.mkApp {
          drv = hakyll-site;
          exePath = "/bin/hakyll-site";
        };

        packages = {
          inherit hakyll-site website;
          default = website;
        };

        # `nix develop` drops you into a shell with a GHC that has the site's
        # dependencies available (so `ghci`/`.ghci` can load ssg/src/Main.hs),
        # the built `hakyll-site` on PATH, and the usual tooling.
        devShells.default = pkgs.haskellPackages.shellFor {
          packages = _: [ hakyll-site ];
          withHoogle = false;
          nativeBuildInputs = [ hakyll-site ] ++ (with pkgs.haskellPackages; [
            cabal-install
            haskell-language-server
            hlint
            ormolu
          ]);
        };

        # `nix fmt` formats the Nix files in this template.
        formatter = pkgs.nixpkgs-fmt;

        # `nix flake check` builds the site.
        checks = { inherit website; };
      }
    );
}
