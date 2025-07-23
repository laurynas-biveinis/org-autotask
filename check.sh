#!/bin/bash

ERRORS=0

echo -n "Checking Markdown files... $(echo ./*.md) "
if mdl --no-verbose ./*.md; then
    echo "OK!"
else
    echo "mdl check failed"
    ERRORS=$((ERRORS + 1))
fi

echo -n "Checking Markdown formatting... $(echo ./*.md) "
if prettier --log-level warn --check ./*.md; then
    echo "OK!"
else
    echo "prettier check for Markdown failed"
    ERRORS=$((ERRORS + 1))
fi

echo -n "Checking YAML formatting... $(echo .github/workflows/*.yml) "
if prettier --log-level warn --check .github/workflows/*.yml; then
    echo "OK!"
else
    echo "prettier check failed!"
    ERRORS=$((ERRORS + 1))
fi

# Final result
if [ $ERRORS -eq 0 ]; then
    echo "All checks passed successfully!"
else
    echo "$ERRORS check(s) failed!"
    exit 1
fi
