# shell.nix
#
# Bridges non-flake tooling to the flake's devShell, so there is a single
# source of truth for the dev environment (defined in flake.nix's
# `devShells.default`). Useful for the VS Code "Nix Env Selector" extension or
# a bare `nix-shell`. If you use flakes directly, prefer `nix develop` — or
# direnv via ./.envrc, which loads the same environment automatically.
(import
  (
    let
      lock = builtins.fromJSON (builtins.readFile ./flake.lock);
      compat = lock.nodes.flake-compat.locked;
    in
    fetchTarball {
      url = "https://github.com/edolstra/flake-compat/archive/${compat.rev}.tar.gz";
      sha256 = compat.narHash;
    }
  )
  { src = ./.; }
).shellNix.default
