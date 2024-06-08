#!/usr/bin/env bash
set -e
cd "$(dirname $0)/.."

gleam update
gleam build
gleam format
gleam test
rm -rf .go-over/
gleam run -- --force
gleam run -- --fake
gleam run