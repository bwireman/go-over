#!/usr/bin/env bash
set -e

cd "$(dirname "$0")/.."

gleam update
gleam run -- 
npm upgrade --ignore-scripts