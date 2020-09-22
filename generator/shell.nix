let
  cfg = import ../nix/default.nix {};
  hp = cfg.haskellPackages;
in
{}:

  hp.shellFor {
    packages = p: [
      p.my-site
    ];

    buildInputs = with hp; [
      cabal-install
      ghcid
      hlint
      hp.my-site
      ormolu
    ];

    withHoogle = true;
  }
