let
  cfg = import ./nix/default.nix { };
in
{ pkgs ? cfg.pkgs }:

pkgs.stdenv.mkDerivation {
  name = "hakyll-nix-template";
  buildInputs = [
    cfg.generator
  ];
  src = cfg.src;

  # https://github.com/jaspervdj/hakyll/issues/614
  # https://github.com/NixOS/nix/issues/318#issuecomment-52986702
  # https://github.com/MaxDaten/brutal-recipes/blob/source/default.nix#L24
  LOCALE_ARCHIVE = pkgs.lib.optionalString (pkgs.buildPlatform.libc == "glibc") "${pkgs.glibcLocales}/lib/locale/locale-archive";
  LANG = "en_US.UTF-8";

  buildPhase = ''
    hakyll-site build
  '';
  installPhase = ''
    mkdir -p "$out/dist"
    cp -r ../dist/* "$out/dist"
  '';
}
