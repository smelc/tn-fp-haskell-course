#!/usr/bin/env bash
#
# Runs checks that can be executed locally

set -eux

cd slides

./build_java.sh
./extract_hs.sh

./check_golden_diff.sh

cd ..
cabal build all
