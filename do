#!/bin/sh

COMMAND=$1
shift

case "$COMMAND" in
  "build")
    exec cabal v2-build exe:boston-haskell-arcade -- "$@"
    ;;

  "docs")
    exec cabal v2-haddock
    ;;

  "run")
    exec cabal v2-run exe:boston-haskell-arcade -- "$@"
    ;;

  *)
    echo "Usage: ./do (build | docs)"
    echo "  build    Build the code"
    echo "  docs     Build local documentation"
    echo "  run      Run the arcade"
esac
