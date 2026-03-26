#!/bin/bash

SEPARATOR="========================================"

# Script to manually verify all error case files
# For each file: print body, run with cargo, show output

echo "$SEPARATOR"
echo "Testing Error Case Files"
echo "$SEPARATOR"
echo ""

for file in test_scripts/error_cases/*.mux; do
    if [[ -f "$file" ]]; then
        filename=$(basename "$file")
        echo "$SEPARATOR"
        echo "FILE: $filename"
        echo "$SEPARATOR"
        echo ""
        echo "--- Contents ---"
        cat "$file"
        echo ""
        echo "--- Output ---"
        cargo run -- run "$file" 2>&1
        exit_code=$?
        echo ""
        echo "Exit code: $exit_code" >&2
        echo ""
        echo "$SEPARATOR"
        echo ""
        read -p "Press enter to continue to next test..."
        echo ""
    fi
done

echo "$SEPARATOR"
echo "All error case tests complete!"
echo "$SEPARATOR"
