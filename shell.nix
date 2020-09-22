let
  sources = import ./nix/sources.nix;
in
{ pkgs ? import sources.nixpkgs {} }:

  let
    cfg = import ./nix/default.nix {};
  in
    pkgs.mkShell {
      buildInputs = cfg.tools;
      shellHook = ''
        ${cfg.ci.pre-commit-check.shellHook}
      '';
    }
