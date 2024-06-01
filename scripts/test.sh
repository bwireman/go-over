#!/usr/bin/env bash
cd "$(dirname $0)/.."

gleam update
gleam format
gleam test
rm -rf .go-over/
gleam run
gleam run -- --skip --force 
gleam run -- --fake
gleam run