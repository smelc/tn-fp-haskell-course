{ system ? builtins.currentSystem, sources ? import ./nix/sources.nix, ghcVersion ? "8107" }:

let
  overlay = _: pkgs: {
    exdown = pkgs.writeShellApplication {
      name = "exdown.py";
      text = ''
        python3 ${sources.exdown}/exdown.py "$@"
      '';
      runtimeInputs = [ pkgs.python38 ];
    };
  };
  pkgs = import sources.nixpkgs { inherit system; overlays = [ overlay ]; };
in with pkgs;

mkShell {
  # Set UTF-8 local so that run-tests can parse GHC's unicode output.
  LANG="C.UTF-8";
  NIX_PATH = "nixpkgs=${pkgs.path}";

  buildInputs = [
    exdown
    haskell.compiler."ghc${ghcVersion}"
    cabal-install
    nix
    bash
    jdk
  ];
}
