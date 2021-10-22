#!/usr/bin/env bash

set -euo pipefail

# TODO the export of the <= operator TxLimits crashes stylish-haskell
stylish-haskell -i $(git ls-files -- 'shardagnostic-consensus*/*.hs' | grep -v -e 'Setup.hs' -e 'Shardagnostic/Consensus/Mempool/TxLimits\.hs')

git diff --exit-code
