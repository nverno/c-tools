#!/usr/bin/env bash

# Fix spacing in C source file
for f in "$@"; do
    if [ ! -d "$f" ]; then
        sed -i 's/( /(/g' "$f"
        sed -i 's/ )/)/g' "$f"
    fi
done
