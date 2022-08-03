#!/usr/bin/env bash
set -e
#
# Generate the .hs files in app, taking the snippets
# from the various *.md files as input

for f in $(ls *.md | grep course)
do
  hs_module_name=$(echo $f | tr -d '-' | sed 's/\.md$//')
  hs_module_name=${hs_module_name^}  # Put first character uppercase
  f_hs="app/${hs_module_name}.hs"
  rm -Rf "$f_hs"
  exdown.py -f hs $f > "$f_hs" || exit 1  # exdown is https://github.com/smelc/exdown
done

cabal build
