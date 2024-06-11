#!/usr/bin/env bash
set -e
yarn run eslint src/ffi.mjs eslint.config.mjs prettier.config.mjs --fix
gleam format