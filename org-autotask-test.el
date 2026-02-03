;;; org-autotask-test.el --- Tests for org-autotask -*- lexical-binding: t -*-

;;; Commentary:
;; Test suite for org-autotask.el using ERT.

;;; Code:

;; Hard dependencies
(require 'ert)
(require 'org-autotask)

(require 'org-clock)
(require 'org-element)

;; Soft dependencies
(defvar org-gcal-cancelled-todo-keyword)

;; Test fixture

(defconst org-autotask--test-contexts
  (vector
   (make-org-autotask-list
    :tag "@c1" :select-char ?a :description "c1 context")
   (make-org-autotask-list
    :tag "@c2" :select-char ?b :description "c2 context")))

;; Non-default list definitions
(defconst org-autotask--test-waitingfor
  (make-org-autotask-list :tag "@wait" :select-char ?w
                          :description "Waiting-for context"))

(defconst org-autotask--test-projects
  (make-org-autotask-list :tag "prj" :select-char ?c :description "Projects"))

(defconst org-autotask--test-somedaymaybe
  (make-org-autotask-list :tag "maybesomeday" :select-char ?d
                          :description "Someday/maybe"))

(defun org-autotask--def-val (sym)
  "Get the default value for a `defcustom' SYM."
  (eval (car (get sym 'standard-value))))

(defmacro org-autotask--test-fixture (varlist &rest body)
  "Test fixture for `org-autotask' to bind VARLIST vars and execute BODY forms."
  (declare (indent 1) (debug t))
  `(let* (
          ;; By default, test the defaults
          (org-autotask-contexts (org-autotask--def-val 'org-autotask-contexts))
          (org-autotask-waitingfor
           (org-autotask--def-val 'org-autotask-waitingfor))
          (org-autotask-projects (org-autotask--def-val 'org-autotask-projects))
          (org-autotask-somedaymaybes
           (org-autotask--def-val 'org-autotask-somedaymaybes))
          (org-autotask-keyword-next-action
           (org-autotask--def-val 'org-autotask-keyword-next-action))
          (org-autotask-keyword-done
           (org-autotask--def-val 'org-autotask-keyword-done))
          (org-autotask-keyword-cancelled
           (org-autotask--def-val 'org-autotask-keyword-cancelled))
          (org-autotask-clock-gated-commands
           (org-autotask--def-val 'org-autotask-clock-gated-commands))
          (org-autotask-clock-in-actions
           (org-autotask--def-val 'org-autotask-clock-in-actions))
          (org-use-tag-inheritance nil)
          (org-todo-log-states nil)
          (org-todo-repeat-to-state nil)
          (org-enforce-todo-dependencies nil)
          (org-stuck-projects nil)
          (org-gcal-cancelled-todo-keyword nil)
          ;; FIXME(laurynas): `org-tag-alist'
          (org-todo-keywords
           '((sequence "TODO(t!)" "|" "DONE(d!)" "CANCELLED(c!)")))
          (org-clock-in-hook nil)
          ;; Prevent dangling clock prompts in batch mode
          (org-clock-persist nil)
          (org-clock-auto-clock-resolution nil)
          (current-clock-marker (when (org-clocking-p)
                                  (copy-marker org-clock-marker)))
          ,@varlist)
     (unwind-protect
         (progn
           ,@body)
       (cond (current-clock-marker
              (org-with-point-at current-clock-marker
                (org-clock-in)))
             ((org-clocking-p) (org-clock-out))))))

;; Test `org-autotask-list'

(ert-deftest org-autotask-context-not-tag-basic ()
  "Basic test for `org-autotask-list-not-tag'."
  (org-autotask--test-fixture ()
    (should (equal (org-autotask-list-not-tag org-autotask-waitingfor)
                   "-@waitingfor"))))

;; Test `org-autotask-initialize'

(defun org-autotask--test-tag-alist-construction (initial expected)
  "Helper to test `org-tag-alist' construction.
INITIAL is the starting value of `org-tag-alist'.
EXPECTED is what `org-tag-alist' should be after initialization."
  (org-autotask--test-fixture
      ((org-tag-alist initial)
       (org-autotask-contexts org-autotask--test-contexts)
       (org-autotask-waitingfor org-autotask--test-waitingfor)
       (org-autotask-projects org-autotask--test-projects)
       (org-autotask-somedaymaybes org-autotask--test-somedaymaybe))
    (org-autotask-initialize)
    (should (equal org-tag-alist expected))))

(ert-deftest org-tag-alist-construction-empty ()
  "Test that `org-tag-alist' is properly constructed, when it's empty."
  (org-autotask--test-tag-alist-construction
   nil '((:startgroup)
         ("@c1" . ?a)
         ("@c2" . ?b)
         ("@wait" . ?w)
         (:endgroup)
         ("prj" . ?c)
         ("maybesomeday" . ?d))))

(ert-deftest org-tag-alist-construction-preexisting ()
  "Test that `org-tag-alist' is properly adjusted, when it's non-empty."
  (org-autotask--test-tag-alist-construction
   '(("@x" . ?x)) '((:startgroup)
                    ("@c1" . ?a)
                    ("@c2" . ?b)
                    ("@wait" . ?w)
                    (:endgroup)
                    ("prj" . ?c)
                    ("maybesomeday" . ?d)
                    ("@x" . ?x))))

(ert-deftest org-autotask-keyword-next-action-not-in-org-todo-keywords ()
  "Test that the next action keyword must be present in `org-todo-keywords'."
  (org-autotask--test-fixture ((org-autotask-keyword-next-action "ABSENT"))
    (should-error (org-autotask-initialize))))

(ert-deftest org-autotask-keyword-next-action-absent-but-prefix ()
  "Test that the absent NA keyword is diagnosed when it's a prefix."
  (org-autotask--test-fixture
      ((org-todo-keywords
        '((sequence "TODONE(t!)" "|" "CANCELLED(c!)" "DONE(d!)"))))
    (should-error (org-autotask-initialize))))

(ert-deftest org-autotask-keyword-done-not-in-org-todo-keywords ()
  "Test that the completed keyword must be present in `org-todo-keywords'."
  (org-autotask--test-fixture ((org-autotask-keyword-done "ABSENT"))
    (should-error (org-autotask-initialize))))

(ert-deftest org-autotask-keyword-done-absent-but-prefix ()
  "Test that the absent done keyword is diagnosed when it's a prefix."
  (org-autotask--test-fixture
      ((org-todo-keywords '((sequence "TODO(t!)" "|" "CANCELLED(c!)"
                                      "DONEANDDONE(d!)"))))
    (should-error (org-autotask-initialize))))

(ert-deftest org-autotask-org-todo-repeat-to-state ()
  "Test that `org-todo-repeat-to-state' is initialized correctly."
  (org-autotask--test-fixture ()
    (org-autotask-initialize)
    (should (equal org-todo-repeat-to-state org-autotask-keyword-next-action))))

(ert-deftest org-autotask-org-use-tag-inheritance-t ()
  "Test `org-use-tag-inheritance' when it's t."
  (org-autotask--test-fixture ((org-use-tag-inheritance t))
    (org-autotask-initialize)
    (should (equal org-use-tag-inheritance t))))

(ert-deftest org-autotask-org-use-tag-inheritance-matching-regex ()
  "Test `org-use-tag-inheritance' matching `org-autotask-somedaymaybe-tag'."
  (org-autotask--test-fixture
      ((org-use-tag-inheritance "may.*")
       (org-autotask-somedaymaybes org-autotask--test-somedaymaybe))
    (org-autotask-initialize)
    (should (equal org-use-tag-inheritance "may.*"))))

(ert-deftest org-autotask-org-use-tag-inheritance-not-matching-regex ()
  "Test `org-use-tag-inheritance' not matching `org-autotask-somedaymaybe-tag'."
  (org-autotask--test-fixture
      ((org-use-tag-inheritance "foo.*")
       (org-autotask-somedaymaybes org-autotask--test-somedaymaybe))
    (should-error (org-autotask-initialize))))

(ert-deftest org-autotask-org-use-tag-inheritance-add-to-list ()
  "Test adding `org-autotask-somedaymaybe-tag' to `org-use-tag-inheritance'."
  (org-autotask--test-fixture
      ((org-use-tag-inheritance '("foo" "bar"))
       (org-autotask-somedaymaybes org-autotask--test-somedaymaybe))
    (org-autotask-initialize)
    (should (equal org-use-tag-inheritance '("maybesomeday" "foo" "bar")))))

(ert-deftest org-autotask-org-use-tag-inheritance-already-in-list ()
  "Test `org-use-tag-inheritance' containing `org-autotask-somedaymaybe-tag'."
  (org-autotask--test-fixture
      ((org-use-tag-inheritance '("maybesomeday" "bar"))
       (org-autotask-somedaymaybes org-autotask--test-somedaymaybe))
    (should-error (org-autotask-initialize))))

(ert-deftest org-autotask-org-use-tag-inheritance-wrong-type ()
  "Test `org-use-tag-inheritance' being of unrecognized type.."
  (org-autotask--test-fixture
      ((org-use-tag-inheritance 42)
       (org-autotask-somedaymaybes org-autotask--test-somedaymaybe))
    (should-error (org-autotask-initialize))))

(ert-deftest org-autotask-keyword-cancelled-not-in-org-todo-keywords ()
  "Test that the cancelled keyword must be present in `org-todo-keywords'."
  (org-autotask--test-fixture ((org-autotask-keyword-cancelled "ABSENT"))
    (should-error (org-autotask-initialize))))

(ert-deftest org-autotask-keyword-cancelled-absent-but-prefix ()
  "Test that the absent cancelled keyword is diagnosed when it's a prefix."
  (org-autotask--test-fixture
      ((org-autotask-keyword-cancelled "KILL")
       (org-todo-keywords '((sequence "TODO(t!)" "|" "DONE(d!)" "KILLED(k!)"))))
    (should-error (org-autotask-initialize))))

(ert-deftest org-autotask-initialize-org-enforce-todo-dependencies ()
  "Test that `org-enforce-todo-dependencies' is initialized correctly."
  (org-autotask--test-fixture ()
    (org-autotask-initialize)
    (should (equal org-enforce-todo-dependencies t))))

(ert-deftest org-autotask-org-gcal-cancelled-todo-keyword ()
  "Test that `org-gcal-cancelled-todo-keyword' is initialized correctly."
  (org-autotask--test-fixture
      ((org-autotask-keyword-cancelled "KILL")
       (org-todo-keywords '((sequence "TODO" "|" "DONE" "KILL"))))
    (org-autotask-initialize)
    (should (equal org-gcal-cancelled-todo-keyword
                   org-autotask-keyword-cancelled))))

(ert-deftest org-autotask-initialize-org-stuck-projects ()
  "Test that `org-stuck-projects' is properly initialized."
  (org-autotask--test-fixture
      ((org-autotask-projects org-autotask--test-projects)
       (org-autotask-somedaymaybes org-autotask--test-somedaymaybe)
       (org-autotask-keyword-next-action "NEXT")
       (org-todo-keywords
        '((sequence "NEXT(n!)" "|" "DONE(d!)" "CANCELLED(c!)"))))
    (org-autotask-initialize)
    (should (equal org-stuck-projects
                   '("+prj-maybesomeday/!NEXT" ("NEXT") nil "")))))

;; Test `org-autotask-agenda-block'

(ert-deftest org-autotask-agenda-block-one-list ()
  "Test for `org-autotask-agenda-block' with one list."
  (org-autotask--test-fixture
      ((ctx-list (make-org-autotask-list :tag "ctx" :select-char ?c
                                         :description "ctx description"))
       (org-autotask-somedaymaybes org-autotask--test-somedaymaybe)
       (org-autotask-keyword-next-action "DOIT")
       (org-todo-keywords
        '((sequence "DOIT(t!)" "|" "DONE(d!)" "CANCELLED(c!)"))))
    (org-autotask-initialize)
    (should (equal (org-autotask-agenda-block ctx-list)
                   '(tags-todo
                     "ctx-maybesomeday/!DOIT"
                     ((org-agenda-overriding-header "ctx description")
                      (org-agenda-dim-blocked-tasks 'invisible)))))))

(ert-deftest org-autotask-active-todo-search-two-lists ()
  "Test for `org-autotask-agenda-block' with two lists."
  (org-autotask--test-fixture
      ((ctx-list (make-org-autotask-list :tag "ctx" :select-char ?c
                                         :description "ctx description"))
       (ctx-list2 (make-org-autotask-list :tag "foo" :select-char ?f
                                          :description "foo description"))
       (org-autotask-somedaymaybes org-autotask--test-somedaymaybe)
       (org-autotask-keyword-next-action "DOIT")
       (org-todo-keywords
        '((sequence "DOIT(t!)" "|" "DONE(d!)" "CANCELLED(c!)"))))
    (org-autotask-initialize)
    (should (equal (org-autotask-agenda-block (list ctx-list ctx-list2)
                                              "Two contexts description")
                   '(tags-todo
                     "ctx-maybesomeday|foo-maybesomeday/!DOIT"
                     ((org-agenda-overriding-header "Two contexts description")
                      (org-agenda-dim-blocked-tasks 'invisible)))))))

(ert-deftest org-autotask-agenda-basic ()
  "Basic test for `org-autotask-agenda'."
  (org-autotask--test-fixture
      ((ctx-list (make-org-autotask-list :tag "foo" :select-char ?f
                                         :description "Foo description"))
       (org-autotask-somedaymaybes org-autotask--test-somedaymaybe)
       (org-autotask-keyword-next-action "DOIT"))
    (should (equal (org-autotask-agenda ctx-list)
                   '("Foo description" tags-todo "foo-maybesomeday/!DOIT")))))

(ert-deftest org-autotask-agenda-somedaymaybe-basic ()
  "Basic test for `org-autotask-agenda-somedaymaybe'."
  (org-autotask--test-fixture
      ((org-autotask-somedaymaybes org-autotask--test-somedaymaybe))
    (should (equal (org-autotask-agenda-somedaymaybe)
                   '("Someday/maybe" tags-todo "maybesomeday+LEVEL=2"
                     ((org-agenda-dim-blocked-tasks nil)))))))

(ert-deftest org-autotask-active-non-project-tasks-basic ()
  "Basic test for `org-autotask-agenda-active-non-project-tasks'."
  (org-autotask--test-fixture
      ((org-autotask-projects org-autotask--test-projects)
       (org-autotask-somedaymaybes org-autotask--test-somedaymaybe)
       (org-autotask-waitingfor org-autotask--test-waitingfor)
       (org-autotask-keyword-next-action "NEXT"))
    (should (equal (org-autotask-agenda-active-non-project-tasks)
                   '("Non-project next actions" tags-todo
                     "-prj-@wait-maybesomeday/!NEXT"
                     ((org-use-tag-inheritance '("prj" "maybesomeday"))))))))

(ert-deftest org-autotask-agenda-archivable-tasks-basic ()
  "Basic test for `org-autotask-agenda-archivable-tasks'."
  (org-autotask--test-fixture
      ((org-autotask-projects org-autotask--test-projects)
       (org-autotask-keyword-done "COMPLETED")
       (org-autotask-keyword-cancelled "KILL"))
    (should (equal (org-autotask-agenda-archivable-tasks)
                   '(tags "-prj/+COMPLETED|+KILL"
                          ((org-agenda-overriding-header "Archivable tasks")
                           (org-use-tag-inheritance '("prj"))))))))

(ert-deftest org-autotask-agenda-contextless-tasks-basic ()
  "Basic test for `org-autotask-agenda-contextless-tasks'."
  (org-autotask--test-fixture
      ((org-autotask-contexts
        (vector
         (make-org-autotask-list :tag "@home" :select-char ?h
                                 :description "At home")
         (make-org-autotask-list :tag "@work" :select-char ?w
                                 :description "At work")))
       (org-autotask-waitingfor org-autotask--test-waitingfor)
       (org-autotask-projects org-autotask--test-projects)
       (org-autotask-somedaymaybes org-autotask--test-somedaymaybe))
    (should (equal (org-autotask-agenda-contextless-tasks)
                   '(todo
                     "-@home-@work-@wait-prj-maybesomeday"
                     ((org-agenda-overriding-header "Contextless tasks")))))))

;; Test creating and completing tasks

(defmacro org-autotask--buffer-test (varlist &rest body)
  "Set up a temp `org' buffer, bind VARLIST and execute BODY in the fixture."
  (declare (indent 1) (debug t))
  `(with-temp-buffer
     (org-autotask--test-fixture ,varlist
       (org-autotask-initialize)
       (org-mode)
       ,@body)))

(defun org-autotask--check-heading-at-point (title todo-state context)
  "Check `org' title at point having TITLE, TODO-STATE, & tagged for CONTEXT."
  (should (string= (org-get-heading t t) title))
  (should (string= (org-get-todo-state) todo-state))
  (should (equal (org-get-tags) (list (org-autotask-list-tag context)))))

(defun org-autotask--insert-waitingfor-na-with-heading (title)
  "Insert a heading and a waiting-for next action with TITLE."
  (org-insert-todo-heading-respect-content)
  (org-autotask-insert-waiting-for-next-action title))

(defun org-autotask--insert-and-check-waitingfor-na (title)
  "Insert a waiting-for next action with TITLE and check it."
  (org-autotask--insert-waitingfor-na-with-heading title)
  (org-autotask--check-heading-at-point title
                                        org-autotask-keyword-next-action
                                        org-autotask-waitingfor))

(defun org-autotask--insert-and-check-project (title)
  "Insert a project next action with TITLE and check it."
  (org-insert-todo-heading-respect-content)
  (org-autotask-insert-project title)
  (org-autotask--check-heading-at-point title
                                        org-autotask-keyword-next-action
                                        org-autotask-projects))

(ert-deftest org-autotask-insert-waiting-for-next-action-basic ()
  "Basic test for `org-autotask-insert-waiting-for-next-action'."
  (org-autotask--buffer-test ()
    (org-autotask--insert-and-check-waitingfor-na "Test title")))

(ert-deftest org-autotask-insert-waiting-for-next-action-reject-empty ()
  "Test that `org-autotask-insert-waiting-for-next-action' rejects empty title."
  (org-autotask--buffer-test ()
    (org-insert-todo-heading-respect-content)
    (should-error (org-autotask-insert-waiting-for-next-action ""))))

(ert-deftest org-autotask-insert-waiting-for-next-action-custom-state-tag ()
  "Test `org-autotask-insert-waiting-for-next-action' with non-default config."
  (org-autotask--buffer-test
      ((org-autotask-keyword-next-action "NEXT")
       (org-autotask-waitingfor org-autotask--test-waitingfor)
       (org-todo-keywords
        '((sequence "NEXT(n!)" "|" "DONE(d!)" "CANCELLED(c!)"))))
    (org-autotask--insert-and-check-waitingfor-na "Title text")))

(ert-deftest org-autotask-insert-project-basic ()
  "Basic test for `org-autotask-insert-project'."
  (org-autotask--buffer-test ()
    (org-autotask--insert-and-check-project "Test title")))

(ert-deftest org-autotask-insert-project-reject-empty ()
  "Test that `org-autotask-insert-project' rejects empty title."
  (org-autotask--buffer-test ()
    (org-insert-todo-heading-respect-content)
    (should-error (org-autotask-insert-project ""))))

(ert-deftest org-autotask-insert-project-custom-state-tag ()
  "Test `org-autotask-insert-project' with non-default config."
  (org-autotask--buffer-test
      ((org-autotask-keyword-next-action "FOO")
       (org-autotask-projects org-autotask--test-projects)
       (org-todo-keywords '((sequence "FOO" "|" "DONE" "CANCELLED"))))
    (org-autotask--insert-and-check-project "Title text")))

(ert-deftest org-autotask-complete-item-basic ()
  "Basic test for `my-org-complete-item'."
  (org-autotask--buffer-test ()
    (org-autotask--insert-waitingfor-na-with-heading "Test title")
    (should (string= (org-get-todo-state) org-autotask-keyword-next-action))
    (org-autotask-complete-item)
    (org-autotask--check-heading-at-point "Test title"
                                          org-autotask-keyword-done
                                          org-autotask-waitingfor)))

;; Test clock-in automation

(defvar org-autotask--action-call-args)

(defun org-autotask--record-action (action)
  "Record test executing ACTION."
  (push action org-autotask--action-call-args))

(defmacro org-autotask--clock-in-action-test (varlist actions &rest body)
  "A test fixture for clock-in automation.
Variables in VARLIST are bound, BODY is executed, and invoked clock-in actions
are compared with ACTIONS."
  (declare (indent 2) (debug t))
  `(org-autotask--buffer-test ((org-autotask--action-call-args '())
                               ,@varlist)
     (org-insert-todo-heading-respect-content)
     ,@body
     (org-clock-in)
     (org-clock-out)
     (should (equal (reverse org-autotask--action-call-args) ,actions))))

(ert-deftest org-autotask-clock-in-actions-basic ()
  "Basic test for `org-autotask--clock-in-actions' with mock actions."
  (org-autotask--clock-in-action-test
      ((org-autotask-clock-in-actions
        `((:property "URL" :action org-autotask--record-action)
          (:property "APP" :action org-autotask--record-action)
          (:property "SHELL" :action org-autotask--record-action)
          (:property "VISIT" :action org-autotask--record-action)
          (:property "EVAL" :action org-autotask--record-action))))
      '("https://example.com" "TestApp" "echo test"
        "/tmp/test.txt" "(message \"test\")")
    (org-set-property "URL" "https://example.com")
    (org-set-property "APP" "TestApp")
    (org-set-property "SHELL" "echo test")
    (org-set-property "VISIT" "/tmp/test.txt")
    (org-set-property "EVAL" "(message \"test\")")))

(ert-deftest org-autotask-clock-in-actions-multi-value ()
  "Test `org-autotask--clock-in-actions' with multi-value properties."
  (org-autotask--clock-in-action-test
      ((org-autotask-clock-in-actions
        `((:property "URL" :action org-autotask--record-action :multi t))))
      '("https://1.example.com" "https://2.example.com")
    (org-set-property "URL" "https://1.example.com")
    (org-entry-add-to-multivalued-property (point) "URL"
                                           "https://2.example.com")))

(defmacro org-autotask--with-replaced-action-fn (property value action-fn
                                                          &rest body)
  "Clock-in testing harness that logs invocations of the tested ACTION-FN.
For a test Org node, PROPERTY is set to VALUE and BODY forms are executed."
  (declare (indent 3) (debug t))
  `(org-autotask--buffer-test
       ((org-autotask--action-call-args '()))
     (cl-letf (((symbol-function ,action-fn) 'org-autotask--record-action))
       (org-insert-todo-heading-respect-content)
       (org-set-property ,property ,value)
       ,@body)))

(defun org-autotask--clock-in-default-action-test (property value action-fn
                                                            expected)
  "Test the default handler for clock-in automation.
For a test Org node, PROPERTY is set to VALUE and clocked-in, asserting that
ACTION-FN was called with EXPECTED arg."
  (org-autotask--with-replaced-action-fn
      property value action-fn
    (org-clock-in)
    (org-clock-out)
    (should (equal org-autotask--action-call-args expected))))

(ert-deftest org-autotask-clock-in-actions-default-url ()
  "Test `org-autotask-clock-in-actions' default URL action handler."
  (org-autotask--clock-in-default-action-test
   "URL" "https://example.com" 'browse-url '("https://example.com")))

(ert-deftest org-autotask-clock-in-actions-default-app ()
  "Test `org-autotask-clock-in-actions' default APP action handler."
  (org-autotask--with-replaced-action-fn
      "APP" "TestApp" 'shell-command
    (if (eq system-type 'darwin)
        (progn
          (org-clock-in)
          (should (equal org-autotask--action-call-args '("open -a TestApp"))))
      (progn
        (should-error (org-clock-in))
        (should (equal org-autotask--action-call-args '()))))
    (org-clock-out)))

(ert-deftest org-autotask-clock-in-actions-default-shell ()
  "Test `org-autotask-clock-in-actions' default SHELL action handler."
  (org-autotask--clock-in-default-action-test
   "SHELL" "cmd with args" 'shell-command '("cmd with args")))

;; TODO(laurynas): it should be possible to test `find-file' calls directly, but
;; such test does not appear to work.
;; TODO(laurynas): buffer position is not tested
(ert-deftest org-autotask-clock-in-actions-default-visit ()
  "Test `org-autotask-clock-in-actions' default VISIT action handler."
  (org-autotask--clock-in-default-action-test
   "VISIT" "/tmp/path" 'find-file '("/tmp/path")))

;; TODO(laurynas): add a test `org-autotask-clock-in-actions-default-eval' to
;; test EVAL action. It should be possible to test `eval' calls directly, but
;; such test does not appear to work, and mocking `eval' has too many side
;; effects.

(ert-deftest org-autotask-require-clock-on ()
  "Test that `org-autotask-require-clock-on' does nothing with an active clock."
  (org-autotask--buffer-test ()
    (org-insert-todo-heading-respect-content)
    (org-clock-in)
    (org-autotask-require-org-clock)
    (org-clock-out)))

(ert-deftest org-autotask-require-clock-off ()
  "Test `org-autotask-require-clock-on' erroring without an active clock."
  (org-autotask--buffer-test ()
    (org-insert-todo-heading-respect-content)
    (when (org-clocking-p)
      (org-clock-out))
    (should-error (org-autotask-require-org-clock))))

(defvar org-autotask-test--1-called)
(defvar org-autotask-test--2-called)
(defvar org-autotask-test--3-called)

(defun org-autotask-test--1 ()
  "Set `org-autotask-test-1--called' to t."
  (setq org-autotask-test--1-called t))

(defun org-autotask-test--2 ()
  "Set `org-autotask-test-2--called' to t."
  (setq org-autotask-test--2-called t))

(defun org-autotask-test--3 ()
  "Set `org-autotask-test-3--called' to t."
  (setq org-autotask-test--3-called t))

(defun org-autotask-test--undo-clock-gating ()
  "Remove clock gating from the test commands."
  (advice-remove 'org-autotask-test--1 #'org-autotask--require-org-clock)
  (advice-remove 'org-autotask-test--2 #'org-autotask--require-org-clock)
  (advice-remove 'org-autotask-test--3 #'org-autotask--require-org-clock))

(ert-deftest org-autotask-clock-gated-commands-blocked ()
  "Test `org-autotask-clock-gated-commands' blocking when not clocking."
  (org-autotask--test-fixture
      ((org-autotask-clock-gated-commands '(org-autotask-test--1
                                            org-autotask-test--3))
       (org-autotask-test--1-called nil)
       (org-autotask-test--2-called nil)
       (org-autotask-test--3-called nil))
    (org-autotask-initialize)
    (unwind-protect
        (progn
          (when (org-clocking-p)
            (org-clock-out))
          (should-error (org-autotask-test--1))
          (org-autotask-test--2)
          (should-error (org-autotask-test--3))
          (should-not org-autotask-test--1-called)
          (should org-autotask-test--2-called)
          (should-not org-autotask-test--3-called))
      (org-autotask-test--undo-clock-gating))))

(ert-deftest org-autotask-clock-gated-commands-allowed ()
  "Test `org-autotask-clock-gated-commands' allowing commands when clocking."
  (org-autotask--buffer-test
      ((org-autotask-clock-gated-commands '(org-autotask-test--2
                                            org-autotask-test--3))
       (org-autotask-test--1-called nil)
       (org-autotask-test--2-called nil)
       (org-autotask-test--3-called nil))
    (unwind-protect
        (progn
          (org-insert-todo-heading-respect-content)
          (org-clock-in)
          (org-autotask-test--1)
          (org-autotask-test--2)
          (org-autotask-test--3)
          (org-clock-out)
          (should org-autotask-test--1-called)
          (should org-autotask-test--2-called)
          (should org-autotask-test--3-called))
      (org-autotask-test--undo-clock-gating))))

;; Test URL property custom automation helpers

(defmacro org-autotask--with-temp-org-agenda-files (&rest body)
  "Bind variable `org-agenda-files' temporarily and execute BODY forms.
Bind `browse-url' temporarily to be a no-op to avoid opening a browser from
tests."
  (declare (debug t) (indent defun))
  `(org-autotask--buffer-test
       ((temp-file (make-temp-file "org-tst" nil ".org"))
        (temp-file-2 (make-temp-file "org-tst" nil ".org"))
        (org-agenda-files (list temp-file temp-file-2)))
     (unwind-protect
         (cl-letf (((symbol-function 'browse-url) (lambda (_x))))
           ,@body)
       (delete-file temp-file)
       (delete-file temp-file-2))))

(ert-deftest org-autotask-with-url-basic ()
  "Basic test for `org-autotask-with-org-node-with-url'."
  (org-autotask--with-temp-org-agenda-files
    (with-temp-file temp-file
      (org-mode)
      (org-insert-todo-heading-respect-content)
      (insert "Item 0")
      (org-insert-todo-heading-respect-content)
      (insert "Item 1")
      (org-set-property "URL" "https://1.example.com")
      (org-insert-todo-heading-respect-content)
      (insert "Item 2")
      (org-set-property "URL" "https://2.example.com"))
    (let (executed)
      (org-autotask-with-org-node-with-url "https://1.example.com"
        (setq executed t)
        (should (string= (org-entry-get nil "URL")
                         "https://1.example.com"))
        (should (string= (org-get-heading t t) "Item 1")))
      (should executed))))

(ert-deftest org-autotask-with-url-not-found ()
  "Test for `org-autotask-with-org-node-with-url' when URL is not found."
  (org-autotask--with-temp-org-agenda-files
    (with-temp-file temp-file
      (org-mode)
      (org-insert-todo-heading-respect-content)
      (insert "Item 0")
      (org-insert-todo-heading-respect-content)
      (insert "Item 1")
      (org-set-property "URL" "https://1.example.com")
      (org-insert-todo-heading-respect-content)
      (insert "Item 2")
      (org-set-property "URL" "https://2.example.com"))
    (should-error (org-autotask-with-org-node-with-url
                      "https://3.example.com"))))

(ert-deftest org-autotask-with-url-multiple-files ()
  "Test `org-autotask-with-org-node-with-url' across multiple files."
  (org-autotask--with-temp-org-agenda-files
    (with-temp-file temp-file
      (org-mode)
      (org-insert-todo-heading-respect-content)
      (insert "Item 1")
      (org-set-property "URL" "https://1.example.com"))
    (with-temp-file temp-file-2
      (org-mode)
      (org-insert-todo-heading-respect-content)
      (insert "Item 2")
      (org-set-property "URL" "https://2.example.com"))
    (let (executed)
      (org-autotask-with-org-node-with-url "https://2.example.com"
        (setq executed t)
        (should (string= (buffer-file-name) temp-file-2))
        (should (string= (org-entry-get nil "URL")
                         "https://2.example.com"))
        (should (string= (org-get-heading t t) "Item 2")))
      (should executed))))

(ert-deftest org-autotask-clock-in-node-with-url-basic ()
  "Basic test for `org-autotask-clock-in-node-with-url'."
  (org-autotask--with-temp-org-agenda-files
    (with-temp-file temp-file
      (org-mode)
      (org-insert-todo-heading-respect-content)
      (insert "Item 0")
      (org-insert-todo-heading-respect-content)
      (insert "Item 1")
      (org-set-property "URL" "https://1.example.com")
      (org-insert-todo-heading-respect-content)
      (insert "Item 2")
      (org-set-property "URL" "https://2.example.com"))
    (org-autotask-clock-in-node-with-url "https://1.example.com")
    (org-clock-goto)
    (should (org-clocking-p))
    (should (string= (org-entry-get nil "URL")
                     "https://1.example.com"))
    (should (string= (org-get-heading t t) "Item 1"))))

(defun org-autotask--insert-heading-marker (title)
  "Create a new `org' heading with TITLE and return a marker inside it."
  (org-insert-todo-heading-respect-content)
  (insert title)
  (org-back-to-heading)
  (forward-char)
  (point-marker))

(defun org-autotask--should-clocked-heading-pos (marker)
  "Test that current `org' clock position is at MARKER.
The marker can be returned by `org-autotask--insert-heading-marker'."
  (should (org-clocking-p))
  (should (= (save-excursion
               (goto-char (marker-position org-clock-marker))
               (org-back-to-heading)
               (forward-char)
               (point))
             (marker-position marker))))

(ert-deftest org-autotask-with-different-org-clock-no-current-clock ()
  "Test `org-autotask-with-different-org-clock' with no current clock."
  (org-autotask--buffer-test
      (executed item1-mark)
    (org-insert-todo-heading-respect-content)
    (insert "Item 0")
    (setq item1-mark (org-autotask--insert-heading-marker "Item 1"))
    (when (org-clocking-p)
      (org-clock-out))
    (org-autotask-with-different-org-clock
      (setq executed t)
      (org-autotask--should-clocked-heading-pos item1-mark))
    (should executed)
    (should-not (org-clocking-p))))

(ert-deftest org-autotask-with-different-org-clock-with-existing-clock ()
  "Test `org-autotask-with-different-org-clock' with an existing clock."
  (org-autotask--buffer-test
      (executed item0-mark item1-mark)
    (setq item0-mark (org-autotask--insert-heading-marker "Item 0"))
    (setq item1-mark (org-autotask--insert-heading-marker "Item 1"))
    (goto-char (marker-position item0-mark))
    (org-clock-in)
    (outline-next-heading)
    (org-autotask-with-different-org-clock
      (setq executed t)
      (org-autotask--should-clocked-heading-pos item1-mark))
    (should executed)
    (org-autotask--should-clocked-heading-pos item0-mark)
    (org-clock-out)))

(ert-deftest org-autotask-with-different-org-clock-error-exit ()
  "Test `org-autotask-with-different-org-clock' cleaning up on error exit."
  (org-autotask--buffer-test (item0-mark)
    (setq item0-mark (org-autotask--insert-heading-marker "Item 0"))
    (org-insert-todo-heading-respect-content)
    (insert "Item 1")
    (goto-char (marker-position item0-mark))
    (org-clock-in)
    (outline-next-heading)
    (should-not (= (marker-position item0-mark) (point)))
    (should-error
     (org-autotask-with-different-org-clock
       (user-error "Test error")))
    (org-autotask--should-clocked-heading-pos item0-mark)
    (org-clock-out)))

;; Test `org-autotask-open-url-at-point'

(ert-deftest org-autotask-open-url-at-point-basic ()
  "Basic test for `org-autotask-open-url-at-point'."
  (org-autotask--buffer-test (url-opened)
    (cl-letf (((symbol-function 'browse-url)
               (lambda (url) (setq url-opened url))))
      (org-insert-todo-heading-respect-content)
      (insert "Test task")
      (org-set-property "URL" "https://example.com")
      (org-autotask-open-url-at-point)
      (should (equal url-opened "https://example.com")))))

(ert-deftest org-autotask-open-url-at-point-multiple-urls ()
  "Test `org-autotask-open-url-at-point' with multiple URLs."
  (org-autotask--buffer-test (urls-opened)
    (cl-letf (((symbol-function 'browse-url)
               (lambda (url) (push url urls-opened))))
      (org-insert-todo-heading-respect-content)
      (insert "Test task")
      (org-set-property "URL" "https://1.example.com")
      (org-entry-add-to-multivalued-property
       (point) "URL" "https://2.example.com")
      (org-autotask-open-url-at-point)
      ;; When using org-entry-add-to-multivalued-property,
      ;; URLs are space-separated
      (should (member "https://1.example.com" urls-opened))
      (should (member "https://2.example.com" urls-opened))
      (should (= (length urls-opened) 2)))))

(ert-deftest org-autotask-open-url-at-point-no-url ()
  "Test `org-autotask-open-url-at-point' when no URL property exists."
  (org-autotask--buffer-test ()
    (cl-letf (((symbol-function 'browse-url)
               (lambda (_) (error "Should not be called"))))
      (org-insert-todo-heading-respect-content)
      (insert "Test task without URL")
      (let ((messages-buffer (current-buffer)))
        (cl-letf (((symbol-function 'message)
                   (lambda (fmt &rest args)
                     (with-current-buffer messages-buffer
                       (insert (apply #'format fmt args))))))
          (org-autotask-open-url-at-point)
          (should (string-match "No URL property found"
                                (buffer-string))))))))

(ert-deftest org-autotask-open-url-at-point-not-in-org-mode ()
  "Test `org-autotask-open-url-at-point' errors when not in Org mode."
  (with-temp-buffer
    (fundamental-mode)
    (should-error (org-autotask-open-url-at-point) :type 'user-error)))

;; TODO(laurynas): idempotency
;; TODO(laurynas): uniqueness in tags
;; TODO(laurynas): uniqueness in keys
;; TODO(laurynas): uniqueness between contexts and waitingfor

;; Test clock archiving

(defmacro org-autotask-test--with-temp-archive (varlist &rest body)
  "Set up temp Org file with archive location and execute BODY.
VARLIST bindings are added to the fixture.  The following variables are bound:
- `temp-file': the source Org file path
- `archive-file': the archive file path (temp-file_archive)
These files are cleaned up after BODY executes."
  (declare (indent 1) (debug t))
  `(org-autotask--test-fixture
       ((temp-file (make-temp-file "org-clock-archive" nil ".org"))
        (archive-file (concat temp-file "_archive"))
        (org-clock-into-drawer t)
        ,@varlist)
     (unwind-protect
         (progn ,@body)
       (delete-file temp-file)
       (delete-file archive-file))))

(defun org-autotask-test--make-old-clock-string (days-ago)
  "Return a CLOCK entry string for a time DAYS-AGO days in the past."
  (let* ((now (current-time))
         (seconds-per-day (* 24 60 60))
         (old-time (time-subtract now (seconds-to-time
                                       (* days-ago seconds-per-day))))
         (end-time (time-add old-time (seconds-to-time 3600))))
    (format "CLOCK: %s--%s =>  1:00"
            (format-time-string "[%Y-%m-%d %a %H:%M]" old-time)
            (format-time-string "[%Y-%m-%d %a %H:%M]" end-time))))

(defun org-autotask-test--make-old-state-change-string (days-ago
                                                        &optional new-state
                                                        old-state)
  "Return a state change entry string for a time DAYS-AGO days in the past.
NEW-STATE and OLD-STATE default to \"DONE\" and \"TODO\" respectively.
The format matches Org's default `org-log-note-headings' state format."
  (let* ((now (current-time))
         (seconds-per-day (* 24 60 60))
         (old-time (time-subtract now (seconds-to-time
                                       (* days-ago seconds-per-day))))
         (new-state (or new-state "DONE"))
         (old-state (or old-state "TODO")))
    (format "- State %-12s from %-12s %s"
            (format "\"%s\"" new-state)
            (format "\"%s\"" old-state)
            (format-time-string "[%Y-%m-%d %a %H:%M]" old-time))))

(ert-deftest org-autotask-log-archive-old-basic ()
  "Basic test for `org-autotask-log-archive-old'."
  (org-autotask-test--with-temp-archive ()
    (let ((old-clock (org-autotask-test--make-old-clock-string 400)))
      ;; Create source file with an old clock
      (with-temp-file temp-file
        (insert "* Test Heading\n")
        (insert ":LOGBOOK:\n")
        (insert old-clock "\n")
        (insert ":END:\n"))
      ;; Archive old clocks
      (with-current-buffer (find-file-noselect temp-file)
        (let* ((org-archive-location (concat archive-file "::"))
               (count (org-autotask-log-archive-old 365)))
          (should (= count 1))
          ;; Source file should have no CLOCK entries and no empty drawer
          (goto-char (point-min))
          (should-not (re-search-forward "^CLOCK:" nil t))
          (should-not (re-search-forward "^:LOGBOOK:" nil t))
          (should-not (re-search-forward "^:END:" nil t))
          ;; Archive file should have the clock under matching heading
          (should (file-exists-p archive-file))))
      (with-current-buffer (find-file-noselect archive-file)
        (goto-char (point-min))
        (should (re-search-forward "^\\* Test Heading.*:archived_logs:" nil t))
        ;; Should have ARCHIVE_FILE property
        (should (re-search-forward ":ARCHIVE_FILE:" nil t))
        (goto-char (point-min))
        (should (re-search-forward "^CLOCK:" nil t))))))

(ert-deftest org-autotask-log-archive-old-state-change ()
  "Test `org-autotask-log-archive-old' archives old state change entries."
  (org-autotask-test--with-temp-archive ()
    (let ((old-state (org-autotask-test--make-old-state-change-string 400)))
      ;; Create source file with an old state change
      (with-temp-file temp-file
        (insert "* Test Heading\n")
        (insert ":LOGBOOK:\n")
        (insert old-state "\n")
        (insert ":END:\n"))
      ;; Archive old state changes
      (with-current-buffer (find-file-noselect temp-file)
        (let* ((org-archive-location (concat archive-file "::"))
               (count (org-autotask-log-archive-old 365)))
          (should (= count 1))
          ;; Source file should have no state change entries and no empty drawer
          (goto-char (point-min))
          (should-not (re-search-forward "^- State" nil t))
          (should-not (re-search-forward "^:LOGBOOK:" nil t))
          (should-not (re-search-forward "^:END:" nil t))
          ;; Archive file should have the state change under matching heading
          (should (file-exists-p archive-file))))
      (with-current-buffer (find-file-noselect archive-file)
        (goto-char (point-min))
        (should (re-search-forward "^\\* Test Heading.*:archived_logs:" nil t))
        ;; Should have ARCHIVE_FILE property
        (should (re-search-forward ":ARCHIVE_FILE:" nil t))
        (goto-char (point-min))
        (should (re-search-forward "^- State" nil t))))))

(ert-deftest org-autotask-log-archive-old-mixed-clocks-and-state-changes ()
  "Test that both clocks and state changes are archived together."
  (org-autotask-test--with-temp-archive ()
    (let ((old-clock (org-autotask-test--make-old-clock-string 400))
          (old-state (org-autotask-test--make-old-state-change-string 450)))
      ;; Create source file with both clock and state change
      (with-temp-file temp-file
        (insert "* Test Heading\n")
        (insert ":LOGBOOK:\n")
        (insert old-state "\n")
        (insert old-clock "\n")
        (insert ":END:\n"))
      ;; Archive both types
      (with-current-buffer (find-file-noselect temp-file)
        (let* ((org-archive-location (concat archive-file "::"))
               (count (org-autotask-log-archive-old 365)))
          (should (= count 2))
          ;; Source file should have no log entries and no empty drawer
          (goto-char (point-min))
          (should-not (re-search-forward "^- State" nil t))
          (should-not (re-search-forward "^CLOCK:" nil t))
          (should-not (re-search-forward "^:LOGBOOK:" nil t))
          (should-not (re-search-forward "^:END:" nil t))
          (should (file-exists-p archive-file))))
      ;; Archive should have both types under single heading
      (with-current-buffer (find-file-noselect archive-file)
        (goto-char (point-min))
        (should (re-search-forward "^\\* Test Heading.*:archived_logs:" nil t))
        ;; Should have only one heading
        (should-not
         (re-search-forward "^\\* Test Heading.*:archived_logs:" nil t))
        ;; Should have both entry types
        (goto-char (point-min))
        (should (re-search-forward "^- State" nil t))
        (goto-char (point-min))
        (should (re-search-forward "^CLOCK:" nil t))))))

(ert-deftest org-autotask-log-archive-old-preserves-order ()
  "Test that archived entries preserve their original file order."
  (org-autotask-test--with-temp-archive ()
    (let ((old-state-1 (org-autotask-test--make-old-state-change-string 450
                                                                        "DONE"
                                                                        "TODO"))
          (old-clock-1 (org-autotask-test--make-old-clock-string 440))
          (old-state-2 (org-autotask-test--make-old-state-change-string 430
                                                                        "TODO"
                                                                        "WAIT"))
          (old-clock-2 (org-autotask-test--make-old-clock-string 420)))
      ;; Create source file with interleaved state changes and clocks
      (with-temp-file temp-file
        (insert "* Test Heading\n")
        (insert ":LOGBOOK:\n")
        (insert old-state-1 "\n")
        (insert old-clock-1 "\n")
        (insert old-state-2 "\n")
        (insert old-clock-2 "\n")
        (insert ":END:\n"))
      ;; Archive all entries
      (with-current-buffer (find-file-noselect temp-file)
        (let* ((org-archive-location (concat archive-file "::"))
               (count (org-autotask-log-archive-old 365)))
          (should (= count 4))))
      ;; Verify archive preserves original order: state, clock, state, clock
      ;; Sequential re-search-forward calls advance point, so finding all
      ;; patterns in sequence proves the order is preserved
      (with-current-buffer (find-file-noselect archive-file)
        (goto-char (point-min))
        (should (re-search-forward ":LOGBOOK:" nil t))
        (should (re-search-forward "DONE.*from.*TODO" nil t))
        (should (re-search-forward "^CLOCK:" nil t))
        (should (re-search-forward "TODO.*from.*WAIT" nil t))
        (should (re-search-forward "^CLOCK:" nil t))))))

(ert-deftest org-autotask-log-archive-old-no-drawer ()
  "Test `org-autotask-log-archive-old' with `org-clock-into-drawer' as nil."
  (org-autotask-test--with-temp-archive ((org-clock-into-drawer nil))
    (let ((old-clock (org-autotask-test--make-old-clock-string 400)))
      ;; Create source file with clock not in drawer
      (with-temp-file temp-file
        (insert "* Test Heading\n")
        (insert old-clock "\n"))
      ;; Archive old clocks
      (with-current-buffer (find-file-noselect temp-file)
        (let* ((org-archive-location (concat archive-file "::"))
               (count (org-autotask-log-archive-old 365)))
          (should (= count 1))
          ;; Source should have no clock
          (goto-char (point-min))
          (should-not (re-search-forward "^CLOCK:" nil t))
          ;; Archive should have clock without drawer
          (should (file-exists-p archive-file))))
      (with-current-buffer (find-file-noselect archive-file)
        (goto-char (point-min))
        (should (re-search-forward "^\\* Test Heading.*:archived_logs:" nil t))
        (should (re-search-forward "^CLOCK:" nil t))
        ;; No drawer
        (goto-char (point-min))
        (should-not (re-search-forward "^:LOGBOOK:" nil t))))))

(ert-deftest org-autotask-log-archive-old-custom-drawer ()
  "Test `org-autotask-log-archive-old' with custom drawer name."
  (org-autotask-test--with-temp-archive ((org-clock-into-drawer "CLOCKING"))
    (let ((old-clock (org-autotask-test--make-old-clock-string 400)))
      ;; Create source file with clock in custom drawer
      (with-temp-file temp-file
        (insert "* Test Heading\n")
        (insert ":CLOCKING:\n")
        (insert old-clock "\n")
        (insert ":END:\n"))
      ;; Archive old clocks
      (with-current-buffer (find-file-noselect temp-file)
        (let* ((org-archive-location (concat archive-file "::"))
               (count (org-autotask-log-archive-old 365)))
          (should (= count 1))
          ;; Source should have no clock and no drawer
          (goto-char (point-min))
          (should-not (re-search-forward "^CLOCK:" nil t))
          (should-not (re-search-forward "^:CLOCKING:" nil t))
          (should (file-exists-p archive-file))))
      ;; Archive should have custom drawer
      (with-current-buffer (find-file-noselect archive-file)
        (goto-char (point-min))
        (should (re-search-forward "^\\* Test Heading.*:archived_logs:" nil t))
        ;; Should have custom drawer, not LOGBOOK
        (should (re-search-forward "^:CLOCKING:" nil t))
        (should-not (search-forward ":LOGBOOK:" nil t))
        (goto-char (point-min))
        (should (re-search-forward "^CLOCK:" nil t))))))

(ert-deftest org-autotask-log-archive-old-multiple-headings ()
  "Test `org-autotask-log-archive-old' with multiple headings."
  (org-autotask-test--with-temp-archive ()
    (let ((old-clock1 (org-autotask-test--make-old-clock-string 400))
          (old-clock2 (org-autotask-test--make-old-clock-string 500)))
      ;; Create source file with clocks under different headings
      (with-temp-file temp-file
        (insert "* Heading One\n")
        (insert ":LOGBOOK:\n")
        (insert old-clock1 "\n")
        (insert ":END:\n")
        (insert "* Heading Two\n")
        (insert ":LOGBOOK:\n")
        (insert old-clock2 "\n")
        (insert ":END:\n"))
      ;; Archive old clocks
      (with-current-buffer (find-file-noselect temp-file)
        (let* ((org-archive-location (concat archive-file "::"))
               (count (org-autotask-log-archive-old 365)))
          (should (= count 2))
          ;; Source should have no clocks
          (goto-char (point-min))
          (should-not (re-search-forward "^CLOCK:" nil t))
          (should (file-exists-p archive-file))))
      ;; Archive should have both headings
      (with-current-buffer (find-file-noselect archive-file)
        (goto-char (point-min))
        (should (re-search-forward
                 "^\\* Heading One.*:archived_logs:" nil t))
        (goto-char (point-min))
        (should (re-search-forward
                 "^\\* Heading Two.*:archived_logs:" nil t))))))

(ert-deftest org-autotask-log-archive-old-running-clock ()
  "Test that `org-autotask-log-archive-old' does not archive running clocks."
  (org-autotask-test--with-temp-archive ()
    ;; Create source file with a running clock (no end time)
    (with-temp-file temp-file
      (insert "* Test Heading\n")
      (insert ":LOGBOOK:\n")
      ;; Running clock has no end time
      (insert "CLOCK: [2020-01-01 Wed 10:00]\n")
      (insert ":END:\n"))
    ;; Try to archive
    (with-current-buffer (find-file-noselect temp-file)
      (let* ((org-archive-location (concat archive-file "::"))
             (count (org-autotask-log-archive-old 365)))
        (should (= count 0))
        ;; Source should still have the clock
        (goto-char (point-min))
        (should (re-search-forward "^CLOCK:" nil t))
        ;; No archive file created
        (should-not (file-exists-p archive-file))))))

(ert-deftest org-autotask-log-archive-old-no-old-clocks ()
  "Test `org-autotask-log-archive-old' when there are no old clocks."
  (org-autotask-test--with-temp-archive ()
    (let ((recent-clock (org-autotask-test--make-old-clock-string 90)))
      ;; Create source file with only recent clock
      (with-temp-file temp-file
        (insert "* Test Heading\n")
        (insert ":LOGBOOK:\n")
        (insert recent-clock "\n")
        (insert ":END:\n"))
      ;; Try to archive
      (with-current-buffer (find-file-noselect temp-file)
        (let* ((org-archive-location (concat archive-file "::"))
               (count (org-autotask-log-archive-old 365)))
          (should (= count 0))
          ;; Source should still have the clock
          (goto-char (point-min))
          (should (re-search-forward "^CLOCK:" nil t))
          ;; No archive file created
          (should-not (file-exists-p archive-file)))))))

(ert-deftest org-autotask-log-archive-old-old-and-recent ()
  "Test `org-autotask-log-archive-old' archives old, preserves recent."
  (org-autotask-test--with-temp-archive ()
    ;; Create entries at 364 days (not archived with 365-day threshold)
    ;; and ones at 366 days (should be archived).  Using 364 instead of 365
    ;; provides margin for time elapsed between test setup and execution.
    (let ((recent-clock (org-autotask-test--make-old-clock-string 364))
          (old-clock (org-autotask-test--make-old-clock-string 366))
          (recent-state (org-autotask-test--make-old-state-change-string 364))
          (old-state (org-autotask-test--make-old-state-change-string 500)))
      (with-temp-file temp-file
        (insert "* Test Heading\n")
        (insert ":LOGBOOK:\n")
        (insert recent-state "\n")
        (insert old-state "\n")
        (insert recent-clock "\n")
        (insert old-clock "\n")
        (insert ":END:\n"))
      (with-current-buffer (find-file-noselect temp-file)
        (let* ((org-archive-location (concat archive-file "::"))
               (count (org-autotask-log-archive-old 365)))
          ;; Only the old clock and old state change should be archived
          (should (= count 2))
          ;; Source should still have recent clock and recent state change
          (goto-char (point-min))
          (should (re-search-forward "^CLOCK:" nil t))
          (should-not (re-search-forward "^CLOCK:" nil t))
          (goto-char (point-min))
          (should (re-search-forward "^- State" nil t))
          (should-not (re-search-forward "^- State" nil t)))))))

(ert-deftest org-autotask-log-archive-old-nested-heading ()
  "Test `org-autotask-log-archive-old' preserves outline path."
  (org-autotask-test--with-temp-archive ()
    (let ((old-clock (org-autotask-test--make-old-clock-string 400)))
      ;; Create source file with nested headings
      (with-temp-file temp-file
        (insert "* Project A\n")
        (insert "** Task Group\n")
        (insert "*** TODO Specific Task\n")
        (insert ":LOGBOOK:\n")
        (insert old-clock "\n")
        (insert ":END:\n"))
      (with-current-buffer (find-file-noselect temp-file)
        (let* ((org-archive-location (concat archive-file "::"))
               (count (org-autotask-log-archive-old 365)))
          (should (= count 1))
          (should (file-exists-p archive-file))))
      (with-current-buffer (find-file-noselect archive-file)
        (goto-char (point-min))
        ;; Heading should be just the leaf node
        (should (re-search-forward
                 "^\\* Specific Task.*:archived_logs:" nil t))
        ;; ARCHIVE_OLPATH should contain parent path
        (should (re-search-forward
                 ":ARCHIVE_OLPATH: Project A/Task Group" nil t))
        ;; ARCHIVE_FILE should be present
        (goto-char (point-min))
        (should (re-search-forward ":ARCHIVE_FILE:" nil t))))))

(ert-deftest org-autotask-log-archive-old-not-in-org-mode ()
  "Test `org-autotask-log-archive-old' errors when not in Org mode."
  (with-temp-buffer
    (fundamental-mode)
    (should-error (org-autotask-log-archive-old) :type 'user-error)))

(ert-deftest org-autotask-log-archive-old-not-in-org-mode-no-prompt ()
  "Test that interactive call checks mode before prompting for days.
When called interactively with prefix arg from non-Org buffer, the
function should error before prompting for the days threshold."
  (with-temp-buffer
    (fundamental-mode)
    (let ((read-number-called nil))
      (cl-letf (((symbol-function 'read-number)
                 (lambda (&rest _)
                   (setq read-number-called t)
                   365)))
        (let ((current-prefix-arg '(4)))
          (should-error
           (call-interactively 'org-autotask-log-archive-old)
           :type 'user-error))
        (should-not read-number-called)))))

(ert-deftest org-autotask-log-archive-old-accumulates ()
  "Test that repeated archiving accumulates log entries under one heading."
  (org-autotask-test--with-temp-archive ()
    (let ((old-clock (org-autotask-test--make-old-clock-string 400))
          (old-state (org-autotask-test--make-old-state-change-string 500)))
      (with-current-buffer (find-file-noselect temp-file)
        (let ((org-archive-location (concat archive-file "::")))
          ;; First archive: create heading with one clock
          (insert "* Test Heading\n")
          (insert ":LOGBOOK:\n")
          (insert old-clock "\n")
          (insert ":END:\n")
          (save-buffer)
          (org-autotask-log-archive-old 365)
          ;; Second archive: add an old state change and archive again
          (erase-buffer)
          (insert "* Test Heading\n")
          (insert ":LOGBOOK:\n")
          (insert old-state "\n")
          (insert ":END:\n")
          (save-buffer)
          (revert-buffer t t)
          (org-autotask-log-archive-old 365))))
    ;; Verify archive has ONE heading with BOTH entries
    (with-current-buffer (find-file-noselect archive-file)
      (revert-buffer t t)
      (goto-char (point-min))
      ;; Should have exactly one archived_logs heading
      (should (re-search-forward
               "^\\* Test Heading.*:archived_logs:" nil t))
      (should-not (re-search-forward
                   "^\\* Test Heading.*:archived_logs:" nil t))
      ;; Should have exactly one LOGBOOK drawer with clock and state change
      (goto-char (point-min))
      (should (re-search-forward "^:LOGBOOK:" nil t))
      (should-not (re-search-forward "^:LOGBOOK:" nil t))
      (goto-char (point-min))
      (should (re-search-forward "^CLOCK:" nil t))
      (goto-char (point-min))
      (should (re-search-forward "^- State" nil t)))))

(ert-deftest org-autotask-log-archive-old-accumulates-respects-olpath ()
  "Test that accumulation respects different outline paths."
  (org-autotask-test--with-temp-archive ()
    (let ((old-clock1 (org-autotask-test--make-old-clock-string 400))
          (old-clock2 (org-autotask-test--make-old-clock-string 500)))
      (with-current-buffer (find-file-noselect temp-file)
        (let ((org-archive-location (concat archive-file "::")))
          ;; Archive clock from "Project A / Task"
          (insert "* Project A\n")
          (insert "** Task\n")
          (insert ":LOGBOOK:\n")
          (insert old-clock1 "\n")
          (insert ":END:\n")
          (save-buffer)
          (org-autotask-log-archive-old 365)
          ;; Archive clock from "Project B / Task" (same name, different path)
          (erase-buffer)
          (insert "* Project B\n")
          (insert "** Task\n")
          (insert ":LOGBOOK:\n")
          (insert old-clock2 "\n")
          (insert ":END:\n")
          (save-buffer)
          (revert-buffer t t)
          (org-autotask-log-archive-old 365))))
    ;; Verify archive has TWO separate headings (one for each olpath)
    (with-current-buffer (find-file-noselect archive-file)
      (revert-buffer t t)
      (goto-char (point-min))
      ;; Should have two separate "Task" headings
      (should (re-search-forward "^\\* Task.*:archived_logs:" nil t))
      (should (re-search-forward "^\\* Task.*:archived_logs:" nil t))
      ;; Should have different ARCHIVE_OLPATH values
      (goto-char (point-min))
      (should (re-search-forward ":ARCHIVE_OLPATH: Project A" nil t))
      (goto-char (point-min))
      (should (re-search-forward ":ARCHIVE_OLPATH: Project B" nil t)))))

(provide 'org-autotask-test)
;; Local variables:
;; elisp-lint-ignored-validators : ("package-lint")
;; End:
;;; org-autotask-test.el ends here
