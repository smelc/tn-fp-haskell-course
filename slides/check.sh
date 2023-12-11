#!/usr/bin/env bash
#
# Continously build the *.hs files extract from
# snippets of the *.md files

ls *.md | grep course | entr "./extract_hs.sh && cabal build" &

declare -r HS_CHECKER_PID="$!"

cleanup() {
  echo "kill $HS_CHECKER_PID"
  kill $HS_CHECKER_PID
}

trap cleanup EXIT

ls *.md | grep course | entr "./build_java.sh"
