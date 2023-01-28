{
  description = "hakyll-nix-template";

  nixConfig = {
    allow-import-from-derivation = "true";
    bash-prompt = "[nix]λ ";
    extra-substituters = [
      "https://cache.iog.io"
      "https://cache.zw3rk.com" # https://github.com/input-output-hk/haskell.nix/issues/1408
    ];
    extra-trusted-public-keys = [
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
      "loony-tools:pr9m4BkM/5/eSTZlkQyRt57Jz7OMBxNSUiMC4FkcNfk="
    ];
  };

  inputs.haskellNix.url = "github:input-output-hk/haskell.nix";
  inputs.nixpkgs.follows = "haskellNix/nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, nixpkgs, flake-utils, haskellNix }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        overlays = [ haskellNix.overlay
          (final: prev: {
            hakyllProject = final.haskell-nix.project' {
              src = ./ssg;
              compiler-nix-name = "ghc925";
              shell.buildInputs = [
                hakyll-site
              ];
              shell.tools = {
                cabal = "latest";
                hlint = "latest";
                haskell-language-server = "latest";
              };
            };
          })
        ];

        pkgs = import nixpkgs {
          inherit overlays system;
          inherit (haskellNix) config;
        };

        flake = pkgs.hakyllProject.flake {};

        executable = "ssg:exe:hakyll-site";

        hakyll-site = flake.packages.${executable};

        website = pkgs.stdenv.mkDerivation {
          name = "website";
          buildInputs = [];
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
            (pkgs.buildPlatform.libc == "glibc")
            "${pkgs.glibcLocales}/lib/locale/locale-archive";

          buildPhase = ''
            ${flake.packages.${executable}}/bin/hakyll-site build --verbose
          '';

          installPhase = ''
            mkdir -p "$out/dist"
            cp -r dist/* "$out/dist"
          '';
        };

      in flake // rec {
        apps = {
          default = flake-utils.lib.mkApp {
            drv = hakyll-site;
            exePath = "/bin/hakyll-site";
          };
        };

        packages = {
          inherit hakyll-site website;
          default = website;
        };
      }
    );
}
