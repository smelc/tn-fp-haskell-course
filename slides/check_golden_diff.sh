#!/usr/bin/env bash
#
# Verify that generated Course* files do not have git modifications.
# Probably you want to execute this file after 'extract_hs.sh' (for hs)
# or 'build_java.sh' (for java).
#
# Usage: ./check_golden_diff.sh [hs|java]
# Without argument, all generated files are checked.

set -u

usage() {
  echo "Usage: $0 [hs|java]" >&2
  echo "  hs   : check Course*.hs files only" >&2
  echo "  java : check Course*.java files only" >&2
  echo "  (no argument): check both" >&2
}

if [[ "$#" -gt 1 ]]
then
  echo "Too many arguments: $#, expected at most 1" >&2
  usage
  exit 1
fi

case "${1:-}" in
  hs) exts=("hs") ;;
  java) exts=("java") ;;
  "") exts=("hs" "java") ;;
  *)
    echo "Unexpected argument: $1" >&2
    usage
    exit 1
    ;;
esac

rc=0

for ext in "${exts[@]}"
do
  case "$ext" in
    hs) generator="extract_hs.sh" ;;
    *) generator="build_java.sh" ;;
  esac

  for file in Course*."$ext"
  do
    # Guard against the glob not matching anything
    [[ -e "$file" ]] || continue
    # Check if there is any modification to file
    if ! git diff --exit-code "$file"
    then
      echo "$file is modified. Please run $generator and commit the changes."
      rc=1
    fi
  done
done

exit "$rc"
