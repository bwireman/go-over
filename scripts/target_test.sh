#!/usr/bin/env bash
set -e
cd "$(dirname "$0")/.."

YELLOW='\033[1;33m'
NC='\033[0m'

function snooze() {
    echo -e "${YELLOW}😴 Snooze...${NC}"
    sleep "$1"
}

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
gleam run $CMD -- --force --verbose --puller curl
rm -rf .go-over/outdated

snooze 15
# shellcheck disable=SC2086
gleam run $CMD -- --outdated --puller wget

# shellcheck disable=SC2086
gleam test $CMD
