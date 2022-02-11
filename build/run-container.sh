#!/usr/bin/env bash

set -euo pipefail

IMAGE="fossa/haskell-static-alpine:ghc-8.10.4"
TMP_DIR="/tmp/turing-machine"
CMD_FILE="build/build.sh"

docker run --rm \
  -v "$(pwd):${TMP_DIR}" \
  -w "${TMP_DIR}" \
  "$IMAGE" \
  sh "$CMD_FILE"

