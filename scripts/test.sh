#!/usr/bin/env bash
set -e
cd "$(dirname "$0")/.."

GREEN='\033[0;32m'
NC='\033[0m'

gleam check
gleam update
gleam build
gleam format
yarn run eslint src/ffi.mjs  --fix

rm -rf .go-over/
echo -e "${GREEN}==> erlang${NC}"
gleam test --target erlang
gleam run --target erlang  -- --force --outdated

echo -e "${GREEN}==> nodejs${NC}"
gleam test --target javascript --runtime nodejs
gleam run --target javascript --runtime nodejs

echo -e "${GREEN}==> deno${NC}"
gleam test --target javascript --runtime deno
gleam run --target javascript --runtime deno -- --force --outdated

echo -e "${GREEN}==> bun${NC}"
gleam test --target javascript --runtime bun
gleam run --target javascript --runtime bun
