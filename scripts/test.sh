#!/usr/bin/env bash
set -e
cd "$(dirname $0)/.."

gleam update
gleam build
gleam format
yarn run eslint src/ffi.mjs  --fix
echo erlang
gleam test --target erlang
echo bun
gleam test --target javascript --runtime bun
echo nodejs
gleam test --target javascript --runtime nodejs
rm -rf .go-over/
gleam run -- --force
gleam run