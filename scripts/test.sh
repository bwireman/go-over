#!/usr/bin/env bash
set -e
cd "$(dirname "$0")/.."

GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m'

function snooze() {
    echo -e "${YELLOW}😴 Snooze...${NC}"
    sleep "$1"
}

PKG_VERSION=$(grep -o "version.*" gleam.toml)
grep "$PKG_VERSION" src/go_over/util/constants.gleam &> /dev/null || (echo "wrong version should be $PKG_VERSION" ; exit 1)

deno fmt
gleam check
gleam update
gleam build
gleam format


snooze 15
rm -rf .go-over/
echo -e "${GREEN}==> erlang${NC}"
./scripts/target_test.sh erlang

snooze 30
echo -e "${GREEN}==> nodejs${NC}"
./scripts/target_test.sh javascript nodejs

snooze 30
echo -e "${GREEN}==> deno${NC}"
./scripts/target_test.sh javascript deno

snooze 30
echo -e "${GREEN}==> bun${NC}"
./scripts/target_test.sh javascript bun
