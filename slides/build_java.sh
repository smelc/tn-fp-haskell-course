#!/usr/bin/env bash
#
# Generate the .java files here, taking the snippets
# from the various *.md files as input

set -e

for f in course*.md
do
  f_java=$(echo "$f" | tr -d '-' | sed 's/\.md$/.java/')
  f_java="${f_java^}"  # Put first character uppercase
  rm -Rf "$f_java"
  exdown.py -f java "$f" > "$f_java" || exit 1
done

javac ./*.java

RC="$?"

rm -Rf ./*.class

exit "$RC"
