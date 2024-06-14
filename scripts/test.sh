#!/usr/bin/env bash
set -e
cd "$(dirname $0)/.."

gleam check
gleam update
gleam build
gleam format
yarn run eslint src/ffi.mjs  --fix
echo "=> erlang"
gleam test --target erlang
echo "=> bun"
gleam test --target javascript --runtime bun
echo "=> nodejs"
gleam test --target javascript --runtime nodejs
echo "=> deno"
gleam test --target javascript --runtime deno
rm -rf .go-over/
gleam run --target javascript --runtime deno -- --force
gleam run --target javascript --runtime bun
gleam run --target javascript --runtime nodejs
gleam run -- --outdated