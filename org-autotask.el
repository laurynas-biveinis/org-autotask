;;; org-autotask.el --- My GTD setup in `org' -*- lexical-binding: t -*-

;; Copyright (C) 2024-2025 Laurynas Biveinis

;; Version: 0.1
;; URL: https://github.com/laurynas-biveinis/org-autotask
;; Package-Requires: ((emacs "27.1"))
;; Keywords: outlines

;;; Commentary:

;; This is a set of building blocks for a task management implementation in Org
;; following GTD (Getting Things Done), with focus on agenda views for contexts
;; and automation, for clocking and writing own ones in Elisp.  Also includes
;; utilities for working with Org entries.  Assumes familiarity with Org.
;;
;; * Example
;;
;; ** Minimal configuration
;;
;; A sample minimum configuration using the defaults.  Note that it might be
;; useful to store the individual contexts in variables instead of calling
;; `'make-org-autotask-list' directly under `vector' if you want to refer to the
;; specific contexts while building agendas:
;;
;;     (defconst my-online-context
;;       (make-org-autotask-list :tag "@online" :select-char ?i
;;                               :description "Online tasks"))
;;     (defconst my-home-context
;;       (make-org-autotask-list :tag "@home" :select-char ?h
;;                               :description "At home"))
;;     (defconst my-office-context
;;       (make-org-autotask-list :tag "@office" :select-char ?o
;;                               :description "At office"))
;;     (setq org-autotask-contexts
;;       (vector my-online-context my-home-context my-office-context))
;;
;;     (setq org-tag-alist '(("tag" . ?t)))
;;     (setq org-use-tag-inheritance '("tag"))
;;     (setq org-todo-keywords
;;           '(sequence "TODO(t!)" "|" "DONE(d!)" "CANCELLED(c!)"))
;;
;;     (org-autotask-initialize)
;;
;; results in
;;
;;     (symbol-value 'org-use-tag-inheritance)
;;       ("somedaymaybe" "tag")
;;     (symbol-value 'org-tag-alist)
;;       ((:startgroup)
;;        ("@online" . ?i) ("@home" . ?h) ("@office" . ?o) ("@waitingfor" . ?w)
;;        (:endgroup)
;;        ("project" . ?p) ("somedaymaybe" . ?m) ("tag" . ?t))
;;     (symbol-value 'org-todo-repeat-to-state)
;;       "TODO"
;;     (symbol-value 'org-todo-enforce-todo-dependencies)
;;       t
;;     (symbol-value 'org-stuck-projects)
;;       ("+project-somedaymaybe/!TODO" ("TODO") nil "")
;;     (symbol-value 'org-gcal-cancelled-todo-keyword)
;;       "CANCELLED"
;;
;; The last variable comes from `org-gcal'.
;;
;; ** Agendas
;;
;; Org agenda building block helpers can be used as follows:
;;
;;     (setq org-agenda-custom-commands
;;           `(("i" ,@(org-autotask-agenda my-online-context))
;;             ("p" ,@(org-autotask-agenda org-autotask-project-list))
;;             ("s" ,@(org-autotask-agenda-somedaymaybe))
;;             ("n" ,@(org-autotask-agenda-active-non-project-tasks))
;;             ("A" "Custom agenda"
;;              ((agenda "" nil)
;;               ,(org-autotask-agenda-block
;;                 (list my-home-context my-office-context)
;;                 "At home and at office")
;;               ,(org-autotask-agenda-block org-autotask-waitingfor-context)
;;               ,(org-autotask-agenda-archivable-tasks)
;;               ,(org-autotask-agenda-contextless-tasks)))))
;;
;; ** Blocking commands unless the Org clock is running
;;
;; For example, to prevent using Magit if no Org clock is running, add the
;; following before the `org-gtd-initialize' call:
;;
;;     (add-to-list 'org-autotask-clock-gated-commands #'magit-status)
;;     (add-to-list 'org-autotask-clock-gated-commands #'magit-commit)
;;     (add-to-list 'org-autotask-clock-gated-commands #'magit-push)
;;     (add-to-list 'org-autotask-clock-gated-commands #'magit-stage)
;;     (add-to-list 'org-autotask-clock-gated-commands #'magit-unstage)
;;
;; Be careful not to add the clocking-in itself nor some low-level command that
;; would lock you out of Emacs.  To do this for your own commands, the above
;; method still works, but it might be more convenient to call
;; `org-autotask-require-org-clock' from the command itself instead.
;;
;; ** Clocking automation
;;
;; With default `org-autotask-clock-in-actions', on clocking-in any of the tasks
;; below, the actions in their properties will be executed.  Naturally, only Org
;; files you trust should be clocked in, because they can run arbitrary code.
;; Each property can only be given once, except for the `:URL:' one.  If adding
;; new properties to the customization variable, multiple-value ones are
;; specified by `:multi' key.
;;
;;     * TODO Do something at example.com                          :@online:
;;     :PROPERTIES:
;;     :URL: https://example.com
;;     :END:
;;
;;     * TODO Do something at two websites                         :@online:
;;     :PROPERTIES:
;;     :URL: https://1.example.com
;;     :URL+: https://2.example.com
;;     :END:
;;
;;     * TODO Play macOS chess                                       :@home:
;;     :PROPERTIES:
;;     :APP: Chess.app
;;     :END:
;;
;;     * TODO Lock screen and do something away from computer        :@home:
;;     :PROPERTIES:
;;     :SHELL: pmset displaysleepnow
;;     :END:
;;
;;     * TODO Work on a certain file in Emacs                      :@office:
;;     :PROPERTIES:
;;     :VISIT: /path/to/file
;;     :END:
;;
;;     * TODO Work on something that is called by Elisp            :@office:
;;     :PROPERTIES:
;;     :EVAL: (my-work)
;;     :END:
;;
;;     * TODO Combine two actions                                  :@office:
;;     :PROPERTIES:
;;     :URL: https://example.com
;;     :VISIT: /peth/to/file
;;     :END:
;;
;; * Concepts
;;
;; - A list is a collection of items, task or otherwise, as in GTD.  A list has
;;   an Org tag for its items, a quick selection character, and a description.
;;
;; - A context is a place where some TODO items, but not necessarily others, can
;;   be done.  An item can belong to only one context.  Items are assigned to
;;   contexts with Org tags.  One special context is "waiting-for" for tasks
;;   that somebody else has to complete.
;;
;; - A project contains items (subprojects or TODO items) in its subtree.  It is
;;   tagged with the configurable project list-specific tag and also has a TODO
;;   entry keyword.
;;
;; - Someday-maybe items are tagged with their category tag, which is configured
;;   to be inheritable in Org, thus either the items themselves or one of the
;;   outline ancestors have to be tagged with it.  Items can be moved from and
;;   to this state by refiling.
;;
;; * Configuration
;;
;; Since this package expects certain Org configuration, some variables should
;; be left untouched, or they will be overwritten:
;; - `org-todo-repeat-to-state'
;; - `org-enforce-todo-dependencies'
;; - `org-stuck-projects'
;;
;; Some other variables have to be set by user and then will be checked/modified
;; during setup:
;; - `org-todo-keywords' must contain all of the `org-autotask'-configured
;;   keywords.
;; - `org-use-tag-inheritance' must either be t, a string that matches the
;;   someday/maybe tag, or be a list.  If it's a list, the tag for someday/maybe
;;   will be added there.
;; - `org-tag-alist' must not have anything related to contexts, projects, and
;;   someday/maybe, and they will be added to it.
;;
;; Tasks (and some other items such projects) are grouped into lists, as in GTD.
;; The list-related customization is:
;; - `org-autotask-contexts': An (elisp) list of GTD contexts, except for the
;;   waiting-for one.  They, together with the waiting-for context, are mutually
;;   exclusive.
;; - `org-autotask-waitingfor': The GTD waiting-for context.  Defaults to
;;   @waitingfor / w.
;; - `org-autotask-projects': The GTD project list.  Defaults to project / p.
;; - `org-autotask-somedaymaybes': The GTD someday/maybe list.  Defaults to
;;   somedaymaybe / m.
;;
;; For actions, there are three customizable TODO entry keywords:
;; - `org-autotask-keyword-next-action': The keyword for the next action (in the
;;   GTD sense).  Active projects have this keyword too.  Defaults to TODO.
;; - `org-autotask-keyword-done': The keyword for a completed task or project.
;;   Defaults to DONE.
;; - `org-autotask-keyword-cancelled': The keyword for a cancelled task or
;;   project.  Defaults to CANCELLED.
;;
;; For clocking automation:
;; - `org-autotask-clock-gated-commands': A list of commands, which may only be
;;   invoked with an Org clock running.
;; - `org-autotask-clock-in-actions': a list of plists configuring automatic
;;   actions to be executed on clocking-in the node that has one of the
;;   configured properties.  The pre-configured ones are URL, APP, SHELL, VISIT,
;;   & EVAL as shown in the example above.
;;
;; * Usage
;;
;; Set the configuration as described above, then call
;; `org-autotask-initialize'.  Beware that calling it multiple times in the same
;; session may have unexpected results on the Org variables it touches.
;;
;; ** Building Agenda Views
;;
;; There are a few functions that can be used in `org-agenda-custom-commands':
;;
;; - `org-autotask-agenda-block' (contexts &optional header): return a tags-todo
;;   form to be included in a custom agenda view for either a single context or
;;   a list of them.  If a header is not passed and it's a single context, use
;;   its description as the header.
;;
;; - `org-autotask-agenda' (context): return a sublist (everything except the
;;   leading key) for a single `org-agenda-custom-commands' entry.
;;
;; - `org-autotask-agenda-somedaymaybe': return a sublist for a single custom
;;   command entry for the someday/maybe items.
;;
;; - `org-autotask-agenda-active-non-project-tasks': return a sublist for active
;;   next actions that are not under any project.
;;
;; - `org-autotask-agenda-archivable-tasks': return a tags form to be included
;;   in a custom agenda view that shows completed items which are not under any
;;   project.
;;
;; - `org-autotask-agenda-contextless-tasks': return a tags form to be included
;;   in a custom agenda view that shows items which are not tagged with any
;;   context.
;;
;; ** Elisp Automation Library
;;
;; - `org-autotask-require-org-clock': call this from interactive commands to
;;   block them unless an Org clock is running.  For the commands written by
;;   someone else, use `my-org-clock-gated-commands' instead.
;;
;; - `org-autotask-with-org-node-with-url' (url &rest body): a macro to find the
;;   Org node with this URL across Org agenda files and then execute the body
;;   forms there.
;;
;; - `org-autotask-clock-in-node-with-url' (url): find the Org node with this
;;   URL and clock it in.
;;
;; - `org-autotask-with-different-org-clock' (&rest body): a macro to save the
;;   current Org clock state, clock in the current Org node, execute the body
;;   forms, and resume previous clocking, if any.
;;
;; - `org-autotask-insert-project' (title): insert a new project with the given
;;   title at point.
;;
;; - `org-autotask-insert-waiting-for-next-action' (title): insert a new
;;   waiting-for item at point.
;;
;; - `org-autotask-complete-item': mark the item at point (a next action or a
;;   project) as completed.
;;
;; * Comparison with other `org' GTD packages
;;
;; ** org-gtd
;;
;; org-gtd is a full prescriptive GTD implementation, covering the whole
;; workflow.  This package, on the other hand, provides some of the building
;; blocks to build your own implementation without prescribing the whole
;; workflow.
;;
;; ** org-edna
;;
;; org-edna provides dependency management for org tasks for automation.  While
;; this package also focuses on automation, it does not focus on the
;; dependencies much.  Both packages can be used together.

;;; Code:

;; Hard dependencies
(require 'cl-lib)
(require 'org)
(require 'org-agenda)
(require 'org-clock)
(require 'org-element)

;; Soft dependencies
(defvar org-gcal-cancelled-todo-keyword)

;; The GTD list structure, used for contexts, projects, and someday/maybe items.
;;;###autoload
(cl-defstruct (org-autotask-list)
  "A single GTD list, which could be for a context, projects, and someday/maybe
items."
  (tag "" :type string :read-only t :documentation "The `org' tag.")
  (select-char
   ? :type character :read-only t
   :documentation "The `org' quick selection character for the tag.")
  (description "" :type string :read-only t
               :documentation "The description string for this list."))

;;;###autoload
(defun org-autotask-list-not-tag (gtd-list)
  "Get the substring for `org-agenda' blocks to exclude GTD-LIST."
  (concat "-" (org-autotask-list-tag gtd-list)))

;;; Customization

;;;###autoload
(defgroup org-autotask nil
  "Configure `org-autotask'."
  :group 'org)

;; Lists

;;;###autoload
(defcustom org-autotask-contexts nil
  "GTD contexts with `org' tags, quick selection characters, and descriptions.
The tags and the selection keys will be added to as a single group to
`org-tag-alist', together with (`org-autotask-waitingfor-tag' .
`org-autotask-waitingfor-select') by `org-autotask-initialize'."
  :type '(repeat (struct :tag "Context"
                         (string :tag "`org' Tag")
                         (character :tag "Quick selection character")
                         (string :tag "Description")))
  :group 'org-autotask
  :package-version '(org-autotask . "0.1"))

;;;###autoload
(defcustom org-autotask-waitingfor
  (make-org-autotask-list :tag "@waitingfor" :select-char ?w
                          :description "Waiting-for items")
  "The GTD waiting-for context."
  :type '(struct :tag "GTD waiting-for context"
                 (string :tag "`org Tag")
                 (character :tag "Quick selection character")
                 (string :tag "Description"))
  :group 'org-autotask
  :package-version '(org-autotask . "0.1"))

;;;###autoload
(defcustom org-autotask-projects
  (make-org-autotask-list :tag "project" :select-char ?p
                          :description "Projects")
  "The GTD project list."
  :type '(struct :tag "Project list."
                 (string :tag "`org Tag")
                 (character :tag "Quick selection character")
                 (string :tag "Description"))
  :group 'org-autotask
  :package-version '(org-autotask . "0.1"))

;;;###autoload
(defcustom org-autotask-somedaymaybes
  (make-org-autotask-list :tag "somedaymaybe" :select-char ?m
                          :description "Someday/maybe")
  "The GTD someday/maybe list."
  :type '(struct :tag "Someday/maybe item list."
                 (string :tag "`org Tag")
                 (character :tag "Quick selection character")
                 (string :tag "Description"))
  :group 'org-autotask
  :package-version '(org-autotask . "0.1"))

;; Entry keywords

;;;###autoload
(defcustom org-autotask-keyword-next-action "TODO"
  "The TODO entry keyword that designates a next action.
Projects also have this keyword (in addition to `org-autotask-projects' tag.) It
must be present in `org-todo-keywords', either directly or through per-file
configuration, with an optional fast state selection character."
  :type '(string)
  :group 'org-autotask
  :package-version '(org-autotask . "0.1"))

;;;###autoload
(defcustom org-autotask-keyword-done "DONE"
  "The TODO entry keyword that designates a completed task or project.
It must be present in `org-todo-keyword', either directly or thorugh per-file
configuration, with an optional fast state selection character."
  :type '(string)
  :group 'org-autotask
  :package-version '(org-autotask . "0.1"))

;;;###autoload
(defcustom org-autotask-keyword-cancelled "CANCELLED"
  "The TODO entry keyword that designates a cancelled task or project.
It must be present in `org-todo-keywords', either directly or through per-file
configuration, with an optional fast state selection character."
  :type '(string)
  :group 'org-autotask
  :package-version '(org-autotask . "0.1"))

;; Clocking automation

;;;###autoload
(defcustom org-autotask-clock-gated-commands '()
  "List of commands that should be gated by `org-autotask-require-org-clock'."
  :type '(repeat symbol)
  :group 'org-autotask
  :package-version '(org-autotask . "0.1"))

;;;###autoload
(defcustom org-autotask-clock-in-actions
  '((:property "URL" :action browse-url :multi t)
    (:property "APP" :action org-autotask-clock-in-open-macos-app)
    (:property "SHELL" :action shell-command)
    (:property "VISIT" :action org-autotask-clock-in-visit-file)
    (:property "EVAL" :action org-autotask-clock-in-eval))
  "Configuration for actions to perform when clocking in.
Each entry is a plist with `:property', `:action', and optionally `:multi' keys.
`:property' is the name of the Org property to look for.
`:action' is the function to call with the property value.
`:multi', if non-nil, indicates that multiple values are allowed for the
property."
  :type '(repeat (plist :options
                        ((:property (string :tag "Org node property"))
                         (:action (function :tag "Action function"))
                         (:multi (choice (const :tag "Single value" nil)
                                         (const :tag "Multiple values" t))))))
  :group 'org-autotask
  :package-version '(org-autotask . "0.1"))

;; Clocking automation actions

;;;###autoload
(defun org-autotask-clock-in-open-macos-app (app)
  "Open APP on macOS."
  (unless (eq system-type 'darwin)
    (user-error "Only supported under macOS"))
  (shell-command (concat "open -a " app)))

;;;###autoload
(defun org-autotask-clock-in-visit-file (file)
  "Visit FILE and move to the end."
  (find-file file)
  (goto-char (point-max)))

;;;###autoload
(defun org-autotask-clock-in-eval (code)
  "Evaluate Elisp CODE."
  (eval (read code)))

(defun org-autotask--clock-in-actions ()
  "Perform configured actions for the clocked-in task."
  (dolist (action org-autotask-clock-in-actions)
    (let* ((property (plist-get action :property))
           (func (plist-get action :action))
           (multi (plist-get action :multi))
           (values (if multi
                       (org-entry-get-multivalued-property (point) property)
                     (list (org-entry-get (point) property)))))
      (dolist (value values)
        (when value
          (funcall func value))))))

;;;###autoload
(defun org-autotask-require-org-clock ()
  "Return user error if no `org' task is currently clocked in."
  (unless (org-clocking-p)
    (user-error "No org task is clocked-in")))

;;;###autoload
(defun org-autotask-open-url-at-point ()
  "Open URL properties from the current Org entry without clocking in.
This function reads the URL property (and URL+ for additional URLs) from
the Org entry at point and opens them in the browser using `browse-url'.

Unlike the automatic URL opening that happens during clock-in, this
allows manual URL opening for quick reference without starting time tracking."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "Not in an Org buffer"))
  (save-excursion
    (org-back-to-heading t)
    (if-let ((urls (org-entry-get-multivalued-property (point) "URL")))
        (dolist (url urls)
          (browse-url url))
      (message "No URL property found for this entry"))))

;; URL property support for custom automation

(defun org-autotask--org-headline-has-url (headline url)
  "Return the HEADLINE if it has the URL property with the given value."
  (let ((url-property-value (org-element-property :URL headline)))
    (and url-property-value (string= url url-property-value)
         headline)))

(defun org-autotask--find-org-node-with-url-property-in-buffer (url)
  "Find an Org node with a given URL property value in the current buffer."
  (org-element-map (org-element-parse-buffer) 'headline
    (lambda (headline)
      (org-autotask--org-headline-has-url headline url)) nil t))

(defun org-autotask--find-org-node-with-url-property (url)
  "Find the Org node with a given URL property value in Org agenda files."
  (let ((files (org-agenda-files))
        (found nil))
    (while (and files (not found))
      (let* ((file (pop files))
             (buffer (or (find-buffer-visiting file)
                         (find-file-noselect file t)))
             (node (with-current-buffer buffer
                     (org-autotask--find-org-node-with-url-property-in-buffer
                      url))))
        (when node
          (setq found (list :buffer buffer :headline node)))))
    found))

;;;###autoload
(defmacro org-autotask-with-org-node-with-url (url &rest body)
  "Go to the `org' node with the URL property value, execute the forms of BODY."
  (declare (indent 1) (debug t))
  `(let ((org-info (org-autotask--find-org-node-with-url-property ,url)))
     (when (not org-info)
       (user-error "URL %s not found in `org-agenda-files'" ,url))
     (let* ((org-buffer (plist-get org-info :buffer))
            (org-headline (plist-get org-info :headline))
            (headline-pos (org-element-property :begin org-headline)))
       (with-current-buffer org-buffer
         (goto-char headline-pos)
         ,@body))))

;;;###autoload
(defun org-autotask-clock-in-node-with-url (url)
  "Go to the `org' node with the given URL property value and clock it in."
  (org-mark-ring-push)
  (org-autotask-with-org-node-with-url url
    (goto-char headline-pos)
    (org-clock-in)
    (message "Clocking-in the `org' node with %s, use C-c & to go back" url)))

;;;###autoload
(defmacro org-autotask-with-different-org-clock (&rest body)
  "Save the current org clock, clock-in, execute the forms of BODY.
The marker must be at the new clock position."
  (declare (indent defun) (debug t))
  `(let ((current-clock-marker (when (org-clocking-p)
                                 (copy-marker org-clock-marker))))
     (unwind-protect
         (progn
           (org-clock-in)
           ,@body)
       (if current-clock-marker
           (org-with-point-at current-clock-marker
             (org-clock-in))
         (org-clock-out)))))

;; `org' setup
(defun org-autotask--check-keyword-in-org-todo-keywords (keyword)
  "Check that KEYWORD in present in `org-todo-keywords'."
  (unless (seq-some
           (lambda (todo-sequence)
             (seq-some (lambda (keyword-and-char)
                         (when (stringp keyword-and-char)
                           (string= (car (split-string keyword-and-char "("))
                                    keyword)))
                       todo-sequence))
           org-todo-keywords)
    (user-error "'%s' must be present in `org-todo-keywords'" keyword)))

(defun org-autotask--make-org-alist-cons-cell (gtd-list)
  "Convert a GTD-LIST to a cons cell for `org-tag-alist'."
  (cons (org-autotask-list-tag gtd-list)
        (org-autotask-list-select-char gtd-list)))

(defun org-autotask--require-org-clock (&rest _args)
  "Block the command if no `org' task is clocked in."
  (org-autotask-require-org-clock))

;;;###autoload
(defun org-autotask-initialize ()
  "Initialize `org-autotask'.
Checks and modifies `org' configuration:
- `org-todo-keywords' must contain all of the `org-autotask'-configured
  keywords.
- `org-use-tag-inheritance' must either be t, a string that matches the
  someday/maybe tag, or be a list.  If it's a list, the tag for someday/maybe
  will be added there.
- `org-tag-alist' must not have anything related to contexts, projects, and
  someday/maybe, and they will be added to it.

Overwrites Org configuration variables:
- `org-todo-repeat-to-state'
- `org-enforce-todo-dependencies'
- `org-stuck-projects'
- `org-gcal-cancelled-todo-keyword' for `org-gcal'

And set up hooks for clock-in automation.

Multiple calls without resetting the Org variables first will result in
inconsistencies."
  ;; Validate config
  (org-autotask--check-keyword-in-org-todo-keywords
   org-autotask-keyword-next-action)
  (org-autotask--check-keyword-in-org-todo-keywords org-autotask-keyword-done)
  (org-autotask--check-keyword-in-org-todo-keywords
   org-autotask-keyword-cancelled)
  ;; Configure `org'
  (let ((somedaymaybe-tag
         (org-autotask-list-tag org-autotask-somedaymaybes)))
    (cond
     ((eq org-use-tag-inheritance t)
      nil)
     ((stringp org-use-tag-inheritance)
      (unless (string-match-p org-use-tag-inheritance somedaymaybe-tag)
        (user-error "`%s' tag %s does not match `%s' regex %s"
                    "org-autotask-somedaymaybes" "org-use-tag-inheritance"
                    somedaymaybe-tag org-use-tag-inheritance)))
     ((listp org-use-tag-inheritance)
      (when (member somedaymaybe-tag org-use-tag-inheritance)
        (user-error "`%s' tag %s already in `%s' %S"
                    "org-autotask-somedaymaybes" "org-use-tag-inheritance"
                    somedaymaybe-tag org-use-tag-inheritance))
      (push somedaymaybe-tag org-use-tag-inheritance))
     (t (user-error "Don't know how handle `org-use-tag-inheritance' value %S"
                    org-use-tag-inheritance))))
  (setq org-todo-repeat-to-state org-autotask-keyword-next-action)
  (setq org-enforce-todo-dependencies t)
  (setq org-tag-alist
        (append (list (cons :startgroup nil))
                (mapcar #'org-autotask--make-org-alist-cons-cell
                        (append org-autotask-contexts
                                (list org-autotask-waitingfor)))
                (list (cons :endgroup nil))
                (list (org-autotask--make-org-alist-cons-cell
                       org-autotask-projects))
                (list (org-autotask--make-org-alist-cons-cell
                       org-autotask-somedaymaybes))
                org-tag-alist))
  (setq org-stuck-projects `(,(concat "+" (org-autotask-list-tag
                                           org-autotask-projects)
                                      (org-autotask-list-not-tag
                                       org-autotask-somedaymaybes) "/!"
                                      org-autotask-keyword-next-action)
                             (,org-autotask-keyword-next-action) nil ""))
  (add-hook 'org-clock-in-hook #'org-autotask--clock-in-actions)
  ;; Configure `org-gcal'
  (setq org-gcal-cancelled-todo-keyword org-autotask-keyword-cancelled)
  ;; Set up clock gating for commands
  (dolist (cmd org-autotask-clock-gated-commands)
    (advice-add cmd :before #'org-autotask--require-org-clock)))

;; Agenda views
(defun org-autotask--active-todo-search (&rest gtd-lists)
  "Return an `org' search string for next actions in GTD-LISTS."
  (let ((not-somedaymaybe
         (org-autotask-list-not-tag org-autotask-somedaymaybes)))
    (concat (mapconcat (lambda (gtd-list)
                         (concat (org-autotask-list-tag gtd-list)
                                 not-somedaymaybe))
                       gtd-lists "|")
            "/!" org-autotask-keyword-next-action)))

;;;###autoload
(defun org-autotask-agenda-block (gtd-lists &optional header)
  "Return a `tags-todo' block for GTD-LISTS with optional HEADER.
GTD-LISTS can be a single GTD list or their sequence.  If HEADER is not
provided, take it from the description of the only list."
  (let* ((single-gtd-list-p (and (not (sequencep gtd-lists))
                                 (org-autotask-list-p gtd-lists)))
         (gtd-lists-list (if single-gtd-list-p (list gtd-lists) gtd-lists))
         (header-string (or header
                            (and single-gtd-list-p
                                 (org-autotask-list-description gtd-lists)))))
    `(tags-todo
      ,(apply #'org-autotask--active-todo-search gtd-lists-list)
      ((org-agenda-overriding-header ,header-string)
       (org-agenda-dim-blocked-tasks 'invisible)))))

;;;###autoload
(defun org-autotask-agenda (gtd-list)
  "Return an `org-agenda' command part to show active items from GTD-LIST.
TODO(laurynas) example (also to README)."
  (list (org-autotask-list-description gtd-list) 'tags-todo
        (org-autotask--active-todo-search gtd-list)))

;;;###autoload
(defun org-autotask-agenda-somedaymaybe ()
  "Return an `org-agenda' command part to show someday/maybe items.
TODO(laurynas) explanation for LEVEL=2."
  (list (org-autotask-list-description org-autotask-somedaymaybes)
        'tags-todo
        (concat (org-autotask-list-tag org-autotask-somedaymaybes) "+LEVEL=2")
        '((org-agenda-dim-blocked-tasks nil))))

;;;###autoload
(defun org-autotask-agenda-active-non-project-tasks ()
  "Return an `org-agenda' command part to show active non-project next actions."
  (list "Non-project next actions"
        'tags-todo
        (concat (org-autotask-list-not-tag org-autotask-projects)
                (org-autotask-list-not-tag org-autotask-waitingfor)
                (org-autotask-list-not-tag org-autotask-somedaymaybes)
                "/!" org-autotask-keyword-next-action)
        `((org-use-tag-inheritance
           '(,(org-autotask-list-tag org-autotask-projects)
             ,(org-autotask-list-tag org-autotask-somedaymaybes))))))

;;;###autoload
(defun org-autotask-agenda-archivable-tasks ()
  "Return an `org-agenda' command part to show archivable non-project tasks."
  (list 'tags
        (concat (org-autotask-list-not-tag org-autotask-projects) "/+"
                org-autotask-keyword-done "|+" org-autotask-keyword-cancelled)
        `((org-agenda-overriding-header "Archivable tasks")
          (org-use-tag-inheritance '(,(org-autotask-list-tag
                                       org-autotask-projects))))))

;;;###autoload
(defun org-autotask-agenda-contextless-tasks ()
  "Return an `org-agenda' command part to show listless tasks."
  (list 'todo
        (concat
         (apply #'concat (mapcar (lambda (context)
                                   (org-autotask-list-not-tag context))
                                 org-autotask-contexts))
         (org-autotask-list-not-tag org-autotask-waitingfor)
         (org-autotask-list-not-tag org-autotask-projects)
         (org-autotask-list-not-tag org-autotask-somedaymaybes))
        '((org-agenda-overriding-header "Contextless tasks"))))

;; Creating new tasks and completing them
(defun org-autotask--insert-item (title keyword tag)
  "Insert a new `org' item with TITLE, KEYWORD, & TAG at point.
The heading must be already created."
  (when (string-empty-p title)
    (user-error "Title cannot be empty"))
  (insert title)
  (org-todo keyword)
  (org-set-tags tag))

;;;###autoload
(defun org-autotask-insert-project (title)
  "Insert a new project task with TITLE at point.
The heading must be already created."
  (org-autotask--insert-item title org-autotask-keyword-next-action
                             (org-autotask-list-tag org-autotask-projects)))

;;;###autoload
(defun org-autotask-insert-waiting-for-next-action (title)
  "Insert a new next action waiting-for task with TITLE at point.
The heading must be already created."
  (org-autotask--insert-item title org-autotask-keyword-next-action
                             (org-autotask-list-tag org-autotask-waitingfor)))

;;;###autoload
(defun org-autotask-complete-item ()
  "Mark the item (a task or a project) at point as done."
  (org-todo org-autotask-keyword-done))

(provide 'org-autotask)
;;; org-autotask.el ends here
