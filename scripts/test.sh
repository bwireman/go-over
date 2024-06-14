#!/usr/bin/env bash
set -e
cd "$(dirname $0)/.."

GREEN='\033[0;32m'
NC='\033[0m'

gleam check
gleam update
gleam build
gleam format
yarn run eslint src/ffi.mjs  --fix

echo -e "${GREEN}==> erlang${NC}"
gleam test --target erlang

echo -e "${GREEN}==> bun${NC}"
gleam test --target javascript --runtime bun

echo -e "${GREEN}==> nodejs${NC}"
gleam test --target javascript --runtime nodejs

echo -e "${GREEN}==> deno${NC}"
gleam test --target javascript --runtime deno

rm -rf .go-over/
echo -e "${GREEN}==> deno${NC}"
gleam run --target javascript --runtime deno -- --force
echo -e "${GREEN}==> bun${NC}"
gleam run --target javascript --runtime bun
echo -e "${GREEN}==> nodejs${NC}"
gleam run --target javascript --runtime nodejs
echo -e "${GREEN}==> erlang${NC}"
gleam run -- --outdated