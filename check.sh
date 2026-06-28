#!/bin/bash

set -eu -o pipefail

ERRORS=0

readonly SHELL_FILES=(check.sh)
SHELL_SYNTAX_FAILED=0

# Test files are excluded from the Eask package file set, so the no-argument
# eask format/lint commands below do not see them; pass them explicitly.
readonly ELISP_TEST_FILES=(org-autotask-test.el)
ELISP_SYNTAX_FAILED=0

# Elisp

echo -n "Checking Elisp syntax... "
if eask recompile; then
	echo "OK!"
else
	echo "Elisp syntax check failed!"
	ERRORS=$((ERRORS + 1))
	ELISP_SYNTAX_FAILED=1
fi

# Only run indentation if there are no syntax errors
if [ $ELISP_SYNTAX_FAILED -eq 0 ]; then
	echo -n "Running elisp-autofmt... "
	# Note: elisp-autofmt and elisp-lint may disagree on cl-defmacro formatting
	# See https://codeberg.org/ideasman42/emacs-elisp-autofmt/issues/10
	if eask format elisp-autofmt &&
		eask format elisp-autofmt "${ELISP_TEST_FILES[@]}"; then
		echo "OK!"
	else
		echo "elisp-autofmt failed!"
		ERRORS=$((ERRORS + 1))
	fi
else
	echo "Skipping indentation due to syntax errors"
fi

eask clean elc >/dev/null 2>&1 || true

# Only run elisp-lint if there are no errors so far.
#
# elisp-lint runs in advisory (non-strict) mode: eask prints findings but exits
# 0 unless --strict is passed. This is intentional -- elisp-autofmt's formatting
# deliberately disagrees with elisp-lint's indent/whitespace/checkdoc validators
# (see the elisp-autofmt note above), so --strict would fail on formatting we
# have deliberately chosen.
if [ $ERRORS -eq 0 ]; then
	echo -n "Running elisp-lint... "
	if eask lint elisp-lint &&
		eask lint elisp-lint "${ELISP_TEST_FILES[@]}"; then
		echo "OK!"
	else
		echo "elisp-lint failed"
		ERRORS=$((ERRORS + 1))
	fi
else
	echo "Skipping elisp-lint due to previous errors"
fi

eask clean elc >/dev/null 2>&1 || true

# Run Eask keywords lint
echo -n "Running eask lint keywords... "
if eask lint keywords; then
	echo "OK!"
else
	echo "eask lint keywords failed"
	ERRORS=$((ERRORS + 1))
fi

echo -n "Running eask lint regexps... "
if eask lint regexps; then
	echo "OK!"
else
	echo "eask lint regexps failed"
	ERRORS=$((ERRORS + 1))
fi

# Only run ERT tests if there are no Elisp syntax errors
if [ $ELISP_SYNTAX_FAILED -eq 0 ]; then
	echo -n "Running all tests... "
	if eask run script test; then
		echo "OK!"
	else
		echo "ERT tests failed"
		ERRORS=$((ERRORS + 1))
	fi
else
	echo "Skipping ERT tests due to Elisp syntax errors"
fi

# Org

echo -n "Checking org files... README.org "
if eask run script org-lint; then
	echo "OK!"
else
	echo "org files check failed"
	ERRORS=$((ERRORS + 1))
fi

# Shell

echo -n "Checking shell syntax... ${SHELL_FILES[*]} "
if bash -n "${SHELL_FILES[@]}"; then
	echo "OK!"
else
	echo "shell syntax check failed!"
	ERRORS=$((ERRORS + 1))
	SHELL_SYNTAX_FAILED=1
fi

if [ $SHELL_SYNTAX_FAILED -eq 0 ]; then
	echo -n "Running shellcheck... ${SHELL_FILES[*]} "
	if shellcheck "${SHELL_FILES[@]}"; then
		echo "OK!"
	else
		echo "shellcheck check failed"
		ERRORS=$((ERRORS + 1))
	fi

	echo -n "Running shfmt to format all shell scripts... ${SHELL_FILES[*]} "
	if shfmt -w "${SHELL_FILES[@]}"; then
		echo "OK!"
	else
		echo "shfmt failed!"
		ERRORS=$((ERRORS + 1))
	fi
else
	echo "Skipping shellcheck, and shfmt due to previous errors"
fi

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
