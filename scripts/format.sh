#!/usr/bin/env bash
set -e
deno fmt
gleam fix
gleam format