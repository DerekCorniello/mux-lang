#!/bin/bash

# Script to manually verify all error case files
# For each file: print body, run with cargo, show output

echo "========================================"
echo "Testing Error Case Files"
echo "========================================"
echo ""

for file in test_scripts/error_cases/*.mux; do
    if [ -f "$file" ]; then
        filename=$(basename "$file")
        echo "========================================"
        echo "FILE: $filename"
        echo "========================================"
        echo ""
        echo "--- Contents ---"
        cat "$file"
        echo ""
        echo "--- Output ---"
        cargo run -- run "$file" 2>&1
        exit_code=$?
        echo ""
        echo "Exit code: $exit_code"
        echo ""
        echo "========================================"
        echo ""
        read -p "Press enter to continue to next test..."
        echo ""
    fi
done

echo "========================================"
echo "All error case tests complete!"
echo "========================================"
