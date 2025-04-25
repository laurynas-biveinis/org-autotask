# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Build/Test Commands

- Run all tests: `make test` or `emacs -batch -l ert -l org-autotask.el -l org-autotask-test.el -f ert-run-tests-batch-and-exit`
- Run a single test: `emacs -batch -l ert -l org-autotask.el -l org-autotask-test.el --eval "(ert-run-test-interactively 'test-name-here)"`
- Byte-compile: `emacs -batch -f batch-byte-compile org-autotask.el`

## Code Style Guidelines

- Lexical binding is used throughout (`-*- lexical-binding: t -*-`)
- Follow Emacs Lisp conventions with kebab-case naming
- Use `cl-lib` for modern Common Lisp constructs (cl-defun, cl-letf, etc.)
- Documentation strings for all public functions
- Use `###autoload` for functions meant to be autoloaded
- Organize code with (require) statements at the top
- Clear error messages should use `user-error`
- Use proper indentation (2 spaces) for Emacs Lisp code
- Follow existing code patterns when adding new functionality
