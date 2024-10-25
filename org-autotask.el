;;; org-autotask.el --- My GTD setup in `org' -*- lexical-binding: t -*-

;; Version: 0.1
;; URL: https://github.com/laurynas-biveinis/org-autotask
;; Package-Requires: ((emacs "27.1"))
;; Keywords: outlines

;;; Commentary:

;; This is a proto-package for my GTD implementation in `org'.

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
(cl-defstruct (org-autotask-list)
  "A single GTD list, which could be for a context, projects, and someday/maybe
items."
  (tag "" :type string :read-only t :documentation "The `org' tag.")
  (select-char ? :type character :read-only t
               :documentation "The `org' quick selection character for the tag.")
  (description "" :type string :read-only t
               :documentation "The description string for this list."))

(defun org-autotask-list-not-tag (gtd-list)
  "Get the substring for `org-agenda' blocks to exclude GTD-LIST."
  (concat "-" (org-autotask-list-tag gtd-list)))

;;; Customization

(defgroup org-autotask nil
  "Configure `org-autotask'."
  :group 'org)

;; Lists

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

(defcustom org-autotask-projects
  (make-org-autotask-list :tag "project" :select-char ?p :description "Projects")
  "The GTD project list."
  :type '(struct :tag "Project list."
                 (string :tag "`org Tag")
                 (character :tag "Quick selection character")
                 (string :tag "Description"))
  :group 'org-autotask
  :package-version '(org-autotask . "0.1"))

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

(defcustom org-autotask-keyword-next-action "TODO"
  "The TODO entry keyword that designates a next action.
Projects also have this keyword (in addition to `org-autotask-projects' tag.) It
must be present in `org-todo-keywords', either directly or through per-file
configuration, with an optional fast state selection character."
  :type '(string)
  :group 'org-autotask
  :package-version '(org-autotask . "0.1"))

(defcustom org-autotask-keyword-done "DONE"
  "The TODO entry keyword that designates a completed task or project.
It must be present in `org-todo-keyword', either directly or thorugh per-file
configuration, with an optional fast state selection character."
  :type '(string)
  :group 'org-autotask
  :package-version '(org-autotask . "0.1"))

(defcustom org-autotask-keyword-cancelled "CANCELLED"
  "The TODO entry keyword that designates a cancelled task or project.
It must be present in `org-todo-keywords', either directly or through per-file
configuration, with an optional fast state selection character."
  :type '(string)
  :group 'org-autotask
  :package-version '(org-autotask . "0.1"))

;; Clocking automation

(defcustom org-autotask-clock-gated-commands '()
  "List of commands that should be gated by `org-autotask-require-org-clock'."
  :type '(repeat symbol)
  :group 'org-autotask
  :package-version '(org-autotask . "0.1"))

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

(defun org-autotask-clock-in-open-macos-app (app)
  "Open APP on macOS."
  ;; FIXME(laurynas): add OS check
  (shell-command (concat "open -a " app)))

(defun org-autotask-clock-in-visit-file (file)
  "Visit FILE and move to the end."
  (find-file file)
  (goto-char (point-max)))

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

(defun org-autotask-require-org-clock ()
  "Return user error if no `org' task is currently clocked in."
  (unless (org-clocking-p)
    (user-error "No org task is clocked-in")))

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
  "Find the Org node with a given URL property value across `org-agenda-files'."
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

(defun org-autotask-clock-in-node-with-url (url)
  "Go to the `org' node with the given URL property value and clock it in."
  (org-mark-ring-push)
  (org-autotask-with-org-node-with-url url
    (goto-char headline-pos)
    (org-clock-in)
    (message "Clocking-in the `org' node with %s, use C-c & to go back" url)))

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

(defun org-autotask-initialize ()
  "Initialize `org-autotask'.
Checks and modifies `org' configuration:
- `org-todo-keywords' must contain all of the `org-autotask'-configured
  keywords.
- `org-use-tag-inheritance' must either be t, a string that matches the
  someday/maybe tag, or be a list. If it's a list, the tag for someday/maybe
  will be added there.
- `org-tag-alist' must not have anything related to contexts, projects, and
  someday/maybe, and they will be added to it.

Overwrites Org configuration variables:
- `org-todo-repeat-to-state'
- `org-enforce-todo-dependencies'
- `org-stuck-projects'

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
        (user-error
         "`org-autotask-somedaymaybes' tag %s does not match `org-use-tag-inheritance' regex %s"
         somedaymaybe-tag org-use-tag-inheritance)))
     ((listp org-use-tag-inheritance)
      (when (member somedaymaybe-tag org-use-tag-inheritance)
        (user-error
         "`org-autotask-somedaymaybes' tag %s already in `org-use-tag-inheritance' %S"
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

(defun org-autotask-agenda-block (gtd-lists &optional header)
  "Return a `tags-todo' block for GTD-LISTS with optional HEADER.
GTD-LISTS can be a single GTD list or their sequence. If HEADER is not provided,
take it from the description of the only list."
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

(defun org-autotask-agenda (gtd-list)
  "Return an `org-agenda' command part to show active items from GTD-LIST.
TODO(laurynas) example (also to README)."
  (list (org-autotask-list-description gtd-list) 'tags-todo
        (org-autotask--active-todo-search gtd-list)))

(defun org-autotask-somedaymaybe-agenda ()
  "Return an `org-agenda' command part to show someday/maybe items.
TODO(laurynas) explanation for LEVEL=2."
  (list (org-autotask-list-description org-autotask-somedaymaybes)
        'tags-todo
        (concat (org-autotask-list-tag org-autotask-somedaymaybes) "+LEVEL=2")
        '((org-agenda-dim-blocked-tasks nil))))

(defun org-autotask-active-non-project-tasks-agenda ()
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

(defun org-autotask-archivable-tasks ()
  "Return an `org-agenda' command part to show archivable non-project tasks."
  (list 'tags
        (concat (org-autotask-list-not-tag org-autotask-projects) "/+"
                org-autotask-keyword-done "|+" org-autotask-keyword-cancelled)
        `((org-agenda-overriding-header "Archivable tasks")
          (org-use-tag-inheritance '(,(org-autotask-list-tag
                                       org-autotask-projects))))))

;; FIXME(laurynas): prefix org-autotask-agenda- here and everywhere applying
(defun org-autotask-contextless-tasks ()
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

(defun org-autotask-insert-project (title)
  "Insert a new project task with TITLE at point.
The heading must be already created."
  (org-autotask--insert-item title org-autotask-keyword-next-action
                             (org-autotask-list-tag org-autotask-projects)))

(defun org-autotask-insert-waiting-for-next-action (title)
  "Insert a new next action waiting-for task with TITLE at point.
The heading must be already created."
  (org-autotask--insert-item title org-autotask-keyword-next-action
                             (org-autotask-list-tag org-autotask-waitingfor)))

(defun org-autotask-complete-item ()
  "Mark the item (a task or a project) at point as done."
  (org-todo org-autotask-keyword-done))

;; TODO(laurynas): What constitutes a project?

(provide 'org-autotask)
;;; org-autotask.el ends here
