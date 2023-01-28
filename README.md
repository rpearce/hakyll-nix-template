# hakyll-nix-template

[Hakyll](https://jaspervdj.be/hakyll/) + [Nix](https://nixos.org) template

## Features

* Build your site into the `./result/dist` folder:
  ```
  λ nix build
  ```
* Start hakyll's dev server that reloads when changes are made:
  ```
  λ nix run . watch
  Listening on http://127.0.0.1:8000
  ...more logs
  ```
* Run any hakyll command through `nix run .`!
  ```
  λ nix run . clean
  Removing dist...
  Removing ssg/_cache...
  Removing ssg/_tmp...
  ```
* Start a development environment that
  * has your shell environment
  * has `hakyll-site` (for building/watching/cleaning hakyll projects)
  * has `hakyll-init` (for generating new projects)
  * can have anything else you put in the `shell.buildInputs` of the
    `hakyllProject` in `flake.nix`
  * is set up to run `ghci` with some defaults and the modules loaded so you can
    make your own changes and test them out in the ghci REPL

  ```
  λ nix develop

  [hakyll]λ hakyll-site build
  ...
  Success

  [hakyll]λ ghci
  ...
  [1 of 1] Compiling Main    ( ssg/src/Main.hs, interpreted )
  ...

  λ >
  ```

### hakyll

All of this is custmomizable, and here are some things that are already done for
you:

* [pandoc](https://github.com/jgm/pandoc/) markdown customization to make it as
  close to GitHub's markdown style as possible
* [`slugger`](https://hackage.haskell.org/package/slugger) module is included that makes nice link URIs based on post titles
* RSS & Atom XML feed generation
* Sitemap generation
* Code syntax highlighting customization
* ...other reasonable defaults

Configure the dev server, cache & tmp directories, and more in
`./ssg/src/Main.hs`.

### Deployment

Deployment is set up through a [GitHub
Action](https://github.com/features/actions) with [cachix](https://cachix.org),
and it deploys to a [GitHub Pages](https://pages.github.com/) branch,
`gh-pages`, when you merge code into your main branch.

Setup information can be found below in the "Cachix" section.

Note: If your main branch's name isn't `main`, ensure `'refs/heads/main'` gets
updated to `'refs/heads/my-main-branch'` in `./github/workflows/main.yml`.

## Setup

### Nix & Flakes

If you don't have [nix](https://nixos.org), follow [the nix installation
instructions](https://nixos.org/download.html).

Once you have nix installed, follow the instructions here to get access to
flakes: https://nixos.wiki/wiki/Flakes.

### Cachix

The `./.github/workflows/main.yml` file builds with help from
[cachix](https://app.cachix.org), so you'll need to generate a signing key to be
able to do this.

1. Create a cache on cachix for your project
1. Follow cachix's instructions to generate a signing keypair
1. Copy the signing keypair value to a new `CACHIX_SIGNING_KEY` secret on
   https://github.com/settings/secrets
