#!/usr/bin/env bash
#
# Verify that Course*.hs files do not have git modification.
# Probably you want to execute this file after 'extract_hs.sh'

for file in Course*.hs
do
  # Check if there is any modification to file
  if ! git diff --exit-code "$file"
  then
    echo "$file is modified. Please run extract_hs.sh and commit the changes."
    exit 1
  fi
done
