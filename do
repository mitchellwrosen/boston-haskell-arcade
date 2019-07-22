#!/bin/sh

COMMAND=$1
if [ ! -z $COMMAND ]; then
  shift
fi

case "$COMMAND" in
  "build")
    exec cabal v2-build all
    ;;

  "dev")
    # Keep running forever, since sometimes ghcid exits by itself, e.g. when you
    # add a new module to the .cabal file before the file exists.
    while true; do
      ghcid -c 'cabal v2-repl -O0 lib:boston-haskell-arcade' --restart boston-haskell-arcade.cabal
      sleep 2
    done
    ;;

  "dev-server")
    while true; do
      ghcid \
        -c 'cabal v2-repl -O0 exe:boston-haskell-arcade-server' \
        --restart server/boston-haskell-arcade-server.cabal
      sleep 2
    done
    ;;

  "dev-server")
    while true; do
      ghcid \
        -c 'cabal v2-repl exe:boston-haskell-arcade-server' \
        --restart server/boston-haskell-arcade-server.cabal
      sleep 2
    done
    ;;

  "docs")
    cabal v2-haddock -O0 2>/dev/null | grep -A1 "Documentation created" | tail -n1
    ;;

  "rebase")
    set -e
    git fetch origin
    git rebase origin/master --auto-stash
    ;;

  "run")
    exec cabal v2-run -O0 exe:boston-haskell-arcade -- "$@"
    ;;

  "run-server")
    exec cabal v2-run -O0 exe:boston-haskell-arcade-server -- "$@"
    ;;

  *)
    echo "Usage: ./do COMMAND"
    echo ""
    echo "Commands:"
    echo "  build              Build the code"
    echo "  dev[-server]       Development - refresh a repl on change"
    echo "  docs               Build local documentation"
    echo "  rebase             Rebase onto origin/master"
    echo "  run [HOST:PORT]    Run the arcade [connected to server]"
    echo "  run-server PORT    Run the arcade server"
esac
