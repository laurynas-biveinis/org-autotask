;; -*- mode: eask; lexical-binding: t -*-

(package "org-autotask"
         "0.1"
         "My GTD setup in `org'")

(author "Laurynas Biveinis" "laurynas.biveinis@gmail.com")
(license "GPLv3")
(website-url "https://github.com/laurynas-biveinis/org-autotask")
(keywords "outlines")

(package-file "org-autotask.el")
;; Declare the file set explicitly so the no-argument eask format/lint
;; commands do not sweep in generated files (e.g. org-autotask-autoloads.el).
;; org-autotask.el is the only library file; org-autotask-test.el is therefore
;; outside the package set and must be passed explicitly to eask format/lint
;; (see check.sh).
(files "org-autotask.el")

(script "test" "eask test ert org-autotask-test.el")

(script "org-lint"
        (concat "emacs -Q --batch "
                "--eval \"(require 'org)\" "
                "--eval \"(require 'org-lint)\" "
                "--eval \"(let ((all-checks-passed t) "
                        "(files '(\\\"README.org\\\"))) "
                    "(dolist (file files all-checks-passed) "
                      "(with-temp-buffer "
                        "(insert-file-contents file) "
                        "(org-mode) "
                        "(let ((results (org-lint))) "
                          "(when results "
                            "(message \\\"Found issues in %s: %S\\\" file results) "
                            "(setq all-checks-passed nil))))) "
                    "(unless all-checks-passed (kill-emacs 1)))\""))

(source "gnu")

(depends-on "emacs" "27.1")
