# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with
code in this repository. These guidelines build on the common user's
guidelines at ~/.claude/CLAUDE.md locally or
<https://raw.githubusercontent.com/laurynas-biveinis/dotfiles/refs/heads/master/ai/.claude/CLAUDE.md>
online.

User-facing documentation is in README.org.

## Build/Test Commands

The project uses [Eask](https://emacs-eask.github.io/) as its build tool.

- Run the full local check pipeline (Elisp syntax, elisp-autofmt, elisp-lint,
  keyword and regular-expression lints, ERT tests, org-lint, plus
  shell/Markdown/YAML): `./check.sh`

- Run all tests: `eask run script test` (equivalently
  `eask test ert org-autotask-test.el`)

- Run a single test:

  ```bash
  emacs -batch -l ert -l org-autotask.el -l org-autotask-test.el \
        --eval "(ert-run-tests-batch-and-exit 'test-name-here)"
  ```

- Byte-compile: `eask compile`

## Code Style Guidelines

- Use `###autoload` for functions meant to be autoloaded
- Clear error messages should use `user-error`
- Follow existing code patterns when adding new functionality
