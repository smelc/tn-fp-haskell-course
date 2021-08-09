#!/usr/bin/env bash

set -eux

cd slides
python3 -m http.server
