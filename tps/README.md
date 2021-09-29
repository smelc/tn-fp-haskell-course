# This is the home of the TPs for the functional programming course of Telecom Nancy

To reproduce a working environment:

* Install the Haskell installer with [ghcup](https://www.haskell.org/ghcup/):
  `curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh`
  This will ask questions:
  - Answer `P` to update your `PATH` automatically.
  - Answer `Y` to install IDE tooling (`haskell-language-server`).
  - Answer `N` to install `stack`, since this project uses the simpler `cabal` build tool

  Once it's done, run `ghcup install ghc 8.10.4` and then `ghcup set ghc 8.10.4`
  to set the expected compiler version.
* Run `cabal build`
* Then install [vscode](https://code.visualstudio.com/) and the
  [Haskell extension](https://marketplace.visualstudio.com/items?itemName=haskell.haskell).
* Happy Haskell hacking! To experiment, hack in one of the `TP*.hs` files;
  see the first line for how to execute them.

This project has a weird configuration to support multiple `Main` entry points.
That is why, if you add a file on your own, you need to update the `hie.yaml` file
(akin to a `.project` in Eclipse terms or an IDEA specification). To do this:

* `cabal install implicit-hie`
* `mv hie.yaml hie.yaml.backup`
* `gen-hie > hie.yaml`
