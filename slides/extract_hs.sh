#!/usr/bin/env bash
#
# Generate the .hs files, taking the snippets
# from the various *.md files as input

set -e

EXDOWN="${EXDOWN:-exdown.py}"

for f in $(ls *.md | grep course)
do
  hs_module_name=$(echo $f | tr -d '-' | sed 's/\.md$//')
  hs_module_name=${hs_module_name^}  # Put first character uppercase
  f_hs="${hs_module_name}.hs"
  rm -Rf "$f_hs"
  $EXDOWN -f hs $f > "$f_hs" || exit 1  # exdown is https://github.com/smelc/exdown
done
