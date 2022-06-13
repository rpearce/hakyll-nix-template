{
  description = "hakyll-nix-template";

  nixConfig.bash-prompt = "[nix]λ ";

  inputs = {
    nixpkgs.url = "nixpkgs/nixos-21.05";

    flake-utils = {
      url = "github:numtide/flake-utils";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { flake-utils, nixpkgs, self }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        config = {};
        overlays = [ (import ./haskell-overlay.nix) ];
        pkgs = import nixpkgs { inherit config overlays system; };
      in rec {
        defaultPackage = packages.website;

        packages = with pkgs.myHaskellPackages; { inherit ssg website; };

        apps.default = flake-utils.lib.mkApp {
          drv = packages.ssg;
          exePath = "/bin/hakyll-site";
        };

        devShell = pkgs.myHaskellPackages.shellFor {
          packages = p: [ p.ssg ];

          buildInputs = with pkgs.myHaskellPackages; [
            ssg

            # Helpful tools for `nix develop` shells
            #
            #ghcid                   # https://github.com/ndmitchell/ghcid
            #haskell-language-server # https://github.com/haskell/haskell-language-server
            #hlint                   # https://github.com/ndmitchell/hlint
            #ormolu                  # https://github.com/tweag/ormolu
          ];

          withHoogle = true;
        };
      }
    );
}
