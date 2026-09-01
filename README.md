[![Haskell build](https://github.com/smelc/tn-fp-haskell-course/actions/workflows/haskell.yml/badge.svg)](https://github.com/smelc/tn-fp-haskell-course/actions/workflows/haskell.yml) 
[![shellcheck](https://github.com/smelc/tn-fp-haskell-course/actions/workflows/shellcheck.yml/badge.svg)](https://github.com/smelc/tn-fp-haskell-course/actions/workflows/shellcheck.yml) 
[![actionlint](https://github.com/smelc/tn-fp-haskell-course/actions/workflows/actionlint.yml/badge.svg)](https://github.com/smelc/tn-fp-haskell-course/actions/workflows/actionlint.yml)
[![Docker](https://github.com/smelc/tn-fp-haskell-course/actions/workflows/docker.yml/badge.svg)](https://github.com/smelc/tn-fp-haskell-course/actions/workflows/docker.yml)

# Course on functional programming - Haskell

This course is being given at [Telecom Nancy](https://smelc.github.io/tn-fp-haskell-course/slides/)
in 2021, 2022, 2023, 2024, 2025, and 2026 in _3A_.

If you have attended this course, please join the [Telecom Nancy Functional Alumnis](https://www.linkedin.com/groups/13114697/) LinkedIn group 👈
which I use to post job ads in companies I work with.

## Instructions for students

### Online version of the slides

Visit [https://smelc.github.io/tn-fp-haskell-course/slides/](https://smelc.github.io/tn-fp-haskell-course/slides/)

### Development intructions for _TPs_

For you to do the _travaux pratiques_ smoothly,
this repository ships a [devcontainer config](.devcontainer/devcontainer.json)
that points to the docker image published by CI on `main`.
In vscode (with the [Dev Containers extension](https://marketplace.visualstudio.com/items?itemName=ms-vscode-remote.remote-containers)):
open this folder, then run **Dev Containers: Reopen in Container** from the
command palette (`Ctrl/Cmd+Shift+P`). The first run pulls the image and mounts
your clone.

Without vscode, you can run the image manually:

```bash
docker pull ghcr.io/smelc/tn-fp-haskell-course:latest
docker run -it --rm -v "$(pwd):/workspaces/tn-fp-haskell-course" -p 8000:8000 -p 6419:6419 ghcr.io/smelc/tn-fp-haskell-course:latest bash
```

If the image is updated, to adopt a new version do:

```bash
docker pull ghcr.io/smelc/tn-fp-haskell-course:latest
```

And the in vscode do **Dev Containers: Rebuild Container**

## Instructions for TP assistants and @smelc

### Instructions for authoring the slides

The [slides](slides) folder uses [remark](https://github.com/gnab/remark).
To display the slides (be it for presenting or for developing them),
you need to serve the `slides` directory with an http server:

```bash
python3 -m http.server  # in slides/
# Or use ./run.sh
```

Now, crawl `localhost:8000` and open the various `html` files.

Note that it's possible to develop the slides offline,
as per the instruction on the
[remark wiki](https://github.com/gnab/remark/wiki#offline-use-without-an-internet-connection).
But I've never done it.

### Development instructions without Docker

These instructions are more [hermetic](https://bazel.build/basics/hermeticity) than
the ones in [tps](./tps/README.md). This is intentional. ⚠️ If you are here to do the _travaux pratiques_,
use the instructions in [tps](./tps/README.md) ⬅️

- The Haskell compiler (GHC) is installed in an isolated manner, in `bin/ghc` (see below)
- This requires [ghcup](https://www.haskell.org/ghcup) in `PATH` and that's all
- When started from this directory, the [vscode Haskell extension](https://github.com/haskell/vscode-haskell)
  will install the required [language server](https://github.com/haskell/haskell-language-server) on its own,
  so nothing to do here.

```bash
mkdir -p bin/{cabal,ghc,hls}
# Instal cabal, this matches PATH_ADD $(pwd)/bin/ghc/bin in .envrc
ghcup install cabal 3.12.1.0 --isolate $(pwd)/bin/cabal
# Populate GHC, this matches PATH_ADD $(pwd)/bin/ghc/bin in .envrc
# Note that GHC's version number is also in .github/workflows/haskell.yml and in docker/Dockerfile
ghcup install ghc 9.10.3 --isolate $(pwd)/bin/ghc
# Note that HLS's version number is also in docker/Dockerfile
ghcup install hls 2.14.0.0 --isolate $(pwd)/bin/hls
```

Because there is a [cabal.project](./cabal.project) file that pins the set of packages to a specific
timestamp, this project is highly reproducible.

To validate code snippets within slides, see [slides/README.md](slides/README.md).

### Development instructions for developing the container itself

To build the image yourself (e.g. to test local changes to the `Dockerfile`),
tag it with the registry name so the devcontainer picks it up:

```bash
docker build -f docker/Dockerfile -t ghcr.io/smelc/tn-fp-haskell-course:latest .
```

---

In the past, this course has been funded by <a href="https://tweag.io/">Tweag</a>
