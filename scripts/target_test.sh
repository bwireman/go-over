#!/usr/bin/env bash
set -e
cd "$(dirname "$0")/.."

if [ -z "$1" ]; then
    echo "Must set target"
    echo "Usage: $0 <erlang|javascript>"
    exit 1
fi

TARGET="$1"
RUNTIME="$2"
if [ "$TARGET" = "erlang" ]; then
    CMD='--target erlang'
else
    if [ -z "$2" ]; then
        echo "Must set runtime"
        echo "Usage: $0 javascript <bun|nodejs|deno>"
        exit 1
    fi

    CMD="--target javascript --runtime $RUNTIME"
fi

# shellcheck disable=SC2086
gleam run $CMD -- --force
rm -rf .go-over/outdated

sleep 5
# shellcheck disable=SC2086
gleam run $CMD -- --outdated

# shellcheck disable=SC2086
gleam test $CMD
