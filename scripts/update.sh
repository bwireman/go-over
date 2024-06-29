#!/usr/bin/env bash
set -e

cd "$(dirname "$0")/.."

gleam update
gleam run -- --outdated
yarn upgrade --ignore-scripts -A --check-files