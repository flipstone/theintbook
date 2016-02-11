#!/bin/sh

cabal sandbox init

if grep ghcjs cabal.config; then
  echo "ghjs already configured"
else
  echo "setting default compiler to ghcjs"
  echo "compiler: ghcjs" >> cabal.config
fi

