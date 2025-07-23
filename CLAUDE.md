# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with
code in this repository. These guidelines build on the common user's
guidelines at ~/.claude/CLAUDE.md locally or
https://raw.githubusercontent.com/laurynas-biveinis/dotfiles/refs/heads/master/ai/.claude/CLAUDE.md
online.

User-facing documentation is in README.org.

This is an Elisp project, and should follow user's Elisp guidelines at
@~/.claude/CLAUDE-elisp.md.

## Build/Test Commands

- Run all tests: `make test` or `emacs -batch -l ert -l org-autotask.el -l org-autotask-test.el -f ert-run-tests-batch-and-exit`
- Run a single test: `emacs -batch -l ert -l org-autotask.el -l org-autotask-test.el --eval "(ert-run-test-interactively 'test-name-here)"`
- Byte-compile: `emacs -batch -f batch-byte-compile org-autotask.el`

## Code Style Guidelines

- Use `###autoload` for functions meant to be autoloaded
- Clear error messages should use `user-error`
- Follow existing code patterns when adding new functionality
