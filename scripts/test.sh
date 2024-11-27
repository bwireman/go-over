#!/usr/bin/env bash
set -e
cd "$(dirname "$0")/.."

GREEN='\033[0;32m'
NC='\033[0m'

deno fmt
gleam check
gleam update
gleam build
gleam format

rm -rf .go-over/
echo -e "${GREEN}==> erlang${NC}"
./scripts/target_test.sh erlang

sleep 15
echo -e "${GREEN}==> nodejs${NC}"
./scripts/target_test.sh javascript nodejs

sleep 15
echo -e "${GREEN}==> deno${NC}"
./scripts/target_test.sh javascript deno

sleep 15
echo -e "${GREEN}==> bun${NC}"
./scripts/target_test.sh javascript bun
