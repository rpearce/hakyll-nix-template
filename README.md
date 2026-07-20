# hakyll-nix-template

[Hakyll](https://jaspervdj.be/hakyll/) + [Nix](https://nixos.org) template

## Quick tips

* Read the tutorial to get started! https://robertwpearce.com/the-hakyll-nix-template-tutorial.html
* All build inputs (GHC, Hakyll, pandoc, ...) come prebuilt from the default
  [NixOS binary cache](https://cache.nixos.org), so a fresh clone builds without
  compiling the toolchain from source and needs no extra substituter setup.
* If you make changes to anything inside of `ssg/`, you'll need to clean the
  hakyll cache and rebuild. This is the preferred series of commands for
  rebuilding (with logs), cleaning the cache, and re-running the dev server:

  ```default
  nix build --print-build-logs && \
    nix run . clean && \
    nix run . watch
  ```

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

  [hakyll-nix]λ hakyll-site build
  ...
  Success

  [hakyll-nix]λ ghci
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

Deployment runs through a [GitHub
Action](https://github.com/features/actions): every push to `main` builds the
site with Nix and publishes it to [GitHub Pages](https://pages.github.com/)
using GitHub's official Pages deployment (`actions/deploy-pages`). It publishes
straight to the Pages CDN — there is no `gh-pages` branch — and authenticates
with a short-lived
[OIDC](https://docs.github.com/en/actions/security-for-github-actions/security-hardening-your-deployments/about-security-hardening-with-openid-connect)
token rather than a long-lived credential.

One-time setup for your fork:

1. Go to **Settings → Pages → Build and deployment → Source** and select
   **GitHub Actions**.
2. Custom domain (optional): set it under **Settings → Pages → Custom domain**.
   This template ships without a `CNAME` file so it deploys cleanly by default;
   if you prefer the file-based approach, add a `CNAME` file under `src/` and a
   copy rule for it in the `forM_` list in `ssg/src/Main.hs`.

Note: If your main branch isn't named `main`, update `branches: [main]` and the
`if: github.ref == 'refs/heads/main'` in `./.github/workflows/main.yml`.

## Setup

### Nix & Flakes

If you don't have [nix](https://nixos.org), follow [the nix installation
instructions](https://nixos.org/download.html).

Once you have nix installed, follow the instructions here to get access to
flakes: https://nixos.wiki/wiki/Flakes.

### Cachix

The `./.github/workflows/main.yml` file pushes build results to
[cachix](https://app.cachix.org) so CI (and anyone building your site) can reuse
cached artifacts instead of rebuilding from source.

1. Create a cache on cachix for your project.
1. **Rename the cache** in `./.github/workflows/main.yml`. It is currently
   hardcoded to this template's cache (`name: hakyll-nix-template`, in the
   "Build with cachix" step); point it at the cache you just created, otherwise
   pushes will fail with a permissions error.
1. Generate a personal auth token with write access to your cache at
   https://app.cachix.org/personal-auth-tokens.
1. Add it as a `CACHIX_AUTH_TOKEN` secret at
   `https://github.com/<you>/<your-repo>/settings/secrets/actions` (this is the
   secret name the workflow reads).
