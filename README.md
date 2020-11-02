# hakyll-nix-template

[![built with nix](https://builtwithnix.org/badge.svg)](https://builtwithnix.org)

[Hakyll](https://jaspervdj.be/hakyll/) + [Nix](https://nixos.org/) template

## Usage

1. Click the "Use this template" button and create your repository
1. Renaming
  1. Rename all instances of `hakyll-nix-template` with the name of your project
  1. Rename `generator/hakyll-nix-template.cabal` to reflect your project name
1. Cachix
  1. Create a cachix cache for your project on https://app.cachix.org
  1. Follow the cachix instructions to generate a signing keypair
  1. Copy the signing keypair value to a new `CACHIX_SIGNING_KEY` secret on
     https://github.com/<MYUSER>/<MY-PROJECT>/settings/secrets
1. Run `nix-build` to build the project and `nix-shell` to open the nix shell
1. If you want to update `niv` and the other pinned dependencies, then in the
   `nix-shell` run `niv init` and then `niv update` and then `exit` to leave the
   nix shell
1. To start an environment for running hakyll commands like `hakyll-site watch`
   (the dev server) and `hakyll-site build`: `cd generator && nix-shell`. You'll
   then have access to the `hakyll-site` executable defined in
   `generator/hakyll-nix-template.cabal`. While you can do `nix-shell` in the
   root and then `cd generator && hakyll-site watch`, you get some more tools if
   you `nix-shell` from within the `generator/` folder.
1. Once you're satisfied, create a branch, push your project up there, and check
   that the GitHub Actions successfully build. If so, you're good to merge your
   project with your main branch.
1. Once you have a successful build on your main branch, open
   https://github.com/<MYUSER>/<MY-PROJECT>/settings and set the GitHub Pages
   branch to be the `gh-pages` branch

## Features

tl;dr: `nix-build` will collect all your pinned dependencies, build your hakyll
site, and output the built site in a `result/` directory. If you set up the
[main GitHub Action](./.github/workflows/main.yml) with what it needs (your
[cachix](https://cachix.org) cache, and your app needs a `CACHIX_SIGNING_KEY`
secret), it will deploy your built site to a `gh-pages` branch.

* Hakyll (see [the generator folder](./generator))
  * Haskell `nix-shell` environment inside the `generator` folder through which
    you can run `hakyll-site watch` and all other hakyll commands, including
    the ability to run `ghci` and load haskell modules for testing
  * Ability to patch hakyll via `hakyll.patch`
  * Ability to provide nixpkgs overrides for packages whose versions need to
    come from [hackage](https://hackage.haskell.org)
  * RSS & Atom XML feed generation
  * Sitemap generation
  * Reasonable pandoc markdown customization to make it as close to GitHub's
    style as possible
  * `Slug.hs` module that makes nice URIs
  * Many other opinionated general website setup features that should be very
    helpful
* Nix
  * Pinned [nixpkgs](https://github.com/NixOS/nixpkgs), [niv](https://github.com/nmattia/niv),
    and [pre-commit-hooks.nix](https://github.com/cachix/pre-commit-hooks.nix)
  * `nix-build` will build your site into a `result/` directory
  * `nix-shell` in the root will give you a shell with the `tools` dependencies
    in [./nix/default.nix](./nix/default.nix)
  * `nix-shell` in [./generator](./generator) will give you a haskell shell with
      your `hakyll-site` available, as well as `ghci`
* Dev linting via [pre-commit-hooks.nix](https://github.com/cachix/pre-commit-hooks.nix)
  * [nix-linter](https://github.com/Synthetica9/nix-linter)
  * [nixpkgs-fmt](https://github.com/nix-community/nixpkgs-fmt)
  * [ormolu](https://github.com/tweag/ormolu)
  * [shellcheck](https://github.com/koalaman/shellcheck)
* Encourages dev use of [lorri](https://github.com/target/lorri)
* Deployment through a [GitHub Actions](https://github.com/features/actions)
  build with [cachix](https://cachix.org) and deploying to
  [GitHub Pages](https://pages.github.com/) via a `gh-pages` branch
