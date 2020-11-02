let
  cfg = import ../nix/default.nix { };
  hp = cfg.haskellPackages;
in
{}:

hp.shellFor {
  packages = p: [
    p.hakyll-nix-template
  ];

  buildInputs = with hp; [
    cabal-install
    ghcid
    hlint
    hp.hakyll-nix-template
    ormolu
  ];

  withHoogle = true;
}
