#!/usr/bin/env bash
set -e
cd "$(dirname $0)/.."

gleam update
gleam build
gleam format
gleam test --target erlang
gleam test --target javascript --runtime bun
gleam test --target javascript --runtime nodejs
rm -rf .go-over/
gleam run -- --force
gleam run -- --fake
gleam run