#!/bin/bash

# Script to test all .mux files in test_scripts/
# For each file: compile with cargo run -- <file>, wait for enter, then run the binary

for file in test_scripts/*.mux; do
    if [ -f "$file" ]; then
        echo "Testing $file"
        cargo run -- "$file"
        read -p "Press enter to run the binary..."
        base=$(basename "$file" .mux)
        echo "Running $base"
        ./test_scripts/"$base"
        read -p "Press enter to continue to next test..."
    fi
done
