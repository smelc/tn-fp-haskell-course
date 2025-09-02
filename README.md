[![Haskell build](https://github.com/smelc/tn-fp-haskell-course/actions/workflows/haskell.yml/badge.svg)](https://github.com/smelc/tn-fp-haskell-course/actions/workflows/haskell.yml)

## Course on functional programming - Haskell

This course was/is being given at [Telecom Nancy](https://smelc.github.io/tn-fp-haskell-course/slides/)
in 2021, 2022, 2023, and 2024 in _3A_.

If you have attended this course, please join the [Telecom Nancy Functional Alumnis](https://www.linkedin.com/groups/13114697/) LinkedIn group 👈
which I use to post job ads in companies I work with.

### Online version

Visit [https://smelc.github.io/tn-fp-haskell-course/slides/](https://smelc.github.io/tn-fp-haskell-course/slides/)

### Offline version, for writing the slides

The [slides](slides)
folder uses [remark](https://github.com/gnab/remark).
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

### Development instruction

These instructions are more [hermetic](https://bazel.build/basics/hermeticity) than
the ones in [tps](./tps/README.md). This is intentional. ⚠️ If you are here to do the _travaux pratiques_,
use the instructions in [tps](./tps/README.md) ⬅️

- The Haskell compiler (GHC) is installed in an isolated manner, in `bin/ghc` (see below)
- This requires [ghcup](https://www.haskell.org/ghcup) in `PATH` and that's all
- When started from this directory, the [vscode Haskell extension](https://github.com/haskell/vscode-haskell)
  will install the required [language server](https://github.com/haskell/haskell-language-server) on its own,
  so nothing to do here.

```
mkdir -p bin/{cabal,ghc,hls}
# Instal cabal, this matches PATH_ADD $(pwd)/bin/ghc/bin in .envrc
ghcup install cabal --isolate $(pwd)/bin/cabal
# Populate GHC, this matches PATH_ADD $(pwd)/bin/ghc/bin in .envrc
# Note that GHC's version number is also in .github/workflows/haskell.yml
ghcup install ghc 9.4.7 --isolate $(pwd)/bin/ghc
ghcup install hls --isolate $(pwd)/bin/hls
```

Because there is a [cabal.project](./cabal.project) file that pins the set of packages to a specific
timestamp, this project is highly reproducible.

To validate code snippets within slides, see [slides/README.md](slides/README.md).

---

This course is funded by my employer: <a href="https://tweag.io/">Tweag</a>
