# This is the home of the TPs for the functional programming course of Telecom Nancy

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
* Run `ghcup install ghc 9.4.7` and then `ghcup set ghc 9.4.7`
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
