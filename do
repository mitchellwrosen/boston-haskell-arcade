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

  "run-server")
    exec cabal v2-run exe:boston-haskell-arcade-server -- "$@"
    ;;

  *)
    echo "Usage: ./do COMMAND"
    echo ""
    echo "Commands:"
    echo "  build              Build the code"
    echo "  docs               Build local documentation"
    echo "  run [HOST:PORT]    Run the arcade [connected to server]"
    echo "  run-server PORT    Run the arcade server"
esac
