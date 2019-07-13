#!/bin/sh

set -e

COMMAND=$1
if [ ! -z $COMMAND ]; then
  shift
fi

case "$COMMAND" in
  "build")
    exec cabal v2-build exe:boston-haskell-arcade -- "$@"
    ;;

  "dev")
    # Keep running forever, since sometimes ghcid exits by itself, e.g. when you
    # add a new module to the .cabal file before the file exists.
    while true; do
      ghcid -c 'cabal v2-repl lib:boston-haskell-arcade' --restart boston-haskell-arcade.cabal
      sleep 2
    done
    ;;

  "docs")
    cabal v2-haddock 2>/dev/null | grep -A1 "Documentation created" | tail -n1
    ;;

  "rebase")
    git stash -q -u
    git fetch origin
    git rebase origin/master
    git stash pop 2>/dev/null || true
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
    echo "  dev                Development - refresh a repl on change"
    echo "  docs               Build local documentation"
    echo "  rebase             Rebase onto origin/master"
    echo "  run [HOST:PORT]    Run the arcade [connected to server]"
    echo "  run-server PORT    Run the arcade server"
esac
