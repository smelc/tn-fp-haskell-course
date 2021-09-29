#!/usr/bin/env bash
#
# Continously build the *.hs files

which ghcid || cabal install ghcid

ghcid
