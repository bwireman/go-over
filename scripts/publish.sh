#!/usr/bin/env bash
set -e

cd "$(dirname "$0")/.."

if [ -z "$1" ]; then
    echo "Must set version for release"
    echo "Usage:" "$0" "<version>"
    exit 1
fi
VER="v$1"

BRANCH="$(git rev-parse --abbrev-ref HEAD)"
if [ "$BRANCH" != "main" ]; then
    echo "Branch must be 'main'"
    exit 1
fi

gleam format
./scripts/update.sh
./scripts/test.sh

if [ -n "$(git status --porcelain)" ]; then
    echo "Working dir mush be clean"
    exit 1
fi

function publish {
    gleam clean
    echo "Tagging" "$VER"
    git tag "$VER"
    git push origin "$VER"
    echo "Publishing to Hex" "$VER"
    HEX_API_KEY=$(cat key._) gleam publish
    echo "🚀"
}

echo "Version set to:" "$VER"
while true; do
    read -rp "Do you wish to publish? [Yn] " yn
    case $yn in
        [Yy]* ) publish; break;;
        [Nn]* ) echo "canceling..." ; exit;;
        * ) publish; break;;
    esac
done
