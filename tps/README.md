# This is the home of the TPs for the functional programming course of Telecom Nancy

## Containerized setup - recommended

The fastest way to get a working environment is to use the docker image
published by CI. OCaml tooling and this repository's code are included.

In vscode (with the [Dev Containers extension](https://marketplace.visualstudio.com/items?itemName=ms-vscode-remote.remote-containers)):
open the repository root, then run **Dev Containers: Reopen in Container**
from the command palette (`Ctrl/Cmd+Shift+P`).
vscode reads [`.devcontainer/devcontainer.json`](../.devcontainer/devcontainer.json),
pulls the image and mounts your clone.

From a terminal inside the container you can then run e.g.:

```shell
cabal run -v0 TP4.hs
```

## Manual setup (without Docker)

To reproduce a working environment, from the repository root, do:

* Install the Haskell installer with [ghcup](https://www.haskell.org/ghcup/):
  `curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh`
  This will ask questions:
  - Answer `P` to update your `PATH` automatically.
  - Answer `Y` to install IDE tooling (`haskell-language-server`).
  - Answer `N` to install `stack`, since this project uses the simpler
    [cabal](https://www.haskell.org/cabal/) build tool
* Install the [System requirements](https://www.haskell.org/ghcup/install/#system-requirements).
  If you are on Linux, this is something like `sudo apt install build-essential curl libffi-dev etc..`
  (see the link for the exact list of packages to install).
* Run `ghcup install ghc 9.10.3` and then `ghcup set ghc 9.10.3`
  to set the expected compiler version.
* Run `cabal build all`. This will take a while as this repository depends on a number
  of fat libraries. Brew some coffee meanwhile.

At this point, everything should be set to work on the command line, for example try:

```shell
cabal run -v0 TP4.hs
```

You should get as output:

```
pyEval "1 + 3" returned: 4
```

Now you can continue with the IDE configuration:

* Install [vscode](https://code.visualstudio.com/) and the
  [Haskell extension](https://marketplace.visualstudio.com/items?itemName=haskell.haskell).
* Launch `vscode` **from the terminal** (`code . &`), **from this repository's root**.
  If `vscode` asks you to install the Haskell Language Server, say yes.
* Happy Haskell hacking! To experiment, hack in one of the `TP*.hs` files;
  see the first line for how to execute them.
