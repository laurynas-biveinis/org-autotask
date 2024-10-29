;;; org-autotask-test.el --- Tests for org-autotask.el. -*- lexical-binding: t -*-

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

(defmacro org-autotask--test-fixture (varlist &rest body)
  "A test fixture for `org-autotask' to bind VARLIST vars and execute BODY forms."
  (declare (indent 1) (debug t))
  `(let ((org-use-tag-inheritance nil)
         (org-todo-log-states nil)
         (org-todo-repeat-to-state nil)
         (org-todo-keywords
          '((sequence "TODO(t!)" "|" "DONE(d!)" "CANCELLED(c!)")))
         (org-clock-in-hook nil)
         (current-clock-marker (when (org-clocking-p)
                                 (copy-marker org-clock-marker)))
         ,@varlist)
     (ignore org-clock-in-hook)
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
  (org-autotask--test-fixture
      ((org-autotask-waitingfor (make-org-autotask-list
                                 :tag "@foo" :select-char ?x
                                 :description "Waiting-for context")))
    (should (equal (org-autotask-list-not-tag org-autotask-waitingfor)
                   "-@foo"))))

;; Test `org-autotask-initialize'

(ert-deftest org-tag-alist-construction-empty ()
  "Test that `org-tag-alist' is properly constructed."
  (org-autotask--test-fixture
      ((org-tag-alist nil)
       (org-autotask-contexts org-autotask--test-contexts)
       (org-autotask-waitingfor
        (make-org-autotask-list
         :tag "@sometag" :select-char ?f :description "Waiting-for context"))
       (org-autotask-projects
        (make-org-autotask-list
         :tag "prj" :select-char ?c :description "Projects"))
       (org-autotask-somedaymaybes
        (make-org-autotask-list
         :tag "maybesomeday" :select-char ?d :description "Someday/maybe")))
    (org-autotask-initialize)
    (should (equal org-tag-alist
                   '((:startgroup)
                     ("@c1" . ?a)
                     ("@c2" . ?b)
                     ("@sometag" . ?f)
                     (:endgroup)
                     ("prj" . ?c)
                     ("maybesomeday" . ?d))))))

(ert-deftest org-tag-alist-construction-preexisting ()
  "Test that `org-tag-alist' is properly constructed, when it's non-empty."
  (org-autotask--test-fixture
      ((org-tag-alist '(("@x" . ?x)))
       (org-autotask-contexts org-autotask--test-contexts)
       (org-autotask-waitingfor
        (make-org-autotask-list
         :tag "@anothertag" :select-char ?x :description "Waiting-for context"))
       (org-autotask-projects
        (make-org-autotask-list
         :tag "foo" :select-char ?f :description "Foos"))
       (org-autotask-somedaymaybes
        (make-org-autotask-list
         :tag "maybesomeday" :select-char ?d :description "Someday/maybe")))
    (org-autotask-initialize)
    (should (equal org-tag-alist
                   '((:startgroup)
                     ("@c1" . ?a)
                     ("@c2" . ?b)
                     ("@anothertag" . ?x)
                     (:endgroup)
                     ("foo" . ?f)
                     ("maybesomeday" . ?d)
                     ("@x" . ?x))))))

(ert-deftest org-autotask-keyword-next-action-not-in-org-todo-keywords ()
  "Test that the next action keyword must be present in `org-todo-keywords'."
  (org-autotask--test-fixture ((org-autotask-keyword-next-action "ABSENT"))
    (should-error (org-autotask-initialize))))

(ert-deftest org-autotask-keyword-next-action-without-selection-char-ok ()
  "Test that the next action keyword is accepted without the selection char."
  (org-autotask--test-fixture ((org-autotask-keyword-next-action "TODO"))
    (org-autotask-initialize)))

(ert-deftest org-autotask-keyword-next-action-absent-but-prefix ()
  "Test that the absent NA keyword is diagnosed when it's a prefix."
  (org-autotask--test-fixture
      ((org-autotask-keyword-next-action "TODO")
       (org-todo-keywords
        '((sequence "TODONE(t!)" "|" "CANCELLED(c!)" "DONE(d!)"))))
    (should-error (org-autotask-initialize))))

(ert-deftest org-autotask-keyword-done-not-in-org-todo-keywords ()
  "Test that the completed keyword must be present in `org-todo-keywords'."
  (org-autotask--test-fixture ((org-autotask-keyword-done "ABSENT"))
    (should-error (org-autotask-initialize))))

(ert-deftest org-autotask-keyword-done-without-selection-char-ok ()
  "Test that the completed keyword is accepted without the selection char."
  (org-autotask--test-fixture ((org-autotask-keyword-done "DONE"))
    (org-autotask-initialize)))

(ert-deftest org-autotask-keyword-done-absent-but-prefix ()
  "Test that the absent done keyword is diagnosed when it's a prefix."
  (org-autotask--test-fixture
      ((org-autotask-keyword-done "DONE")
       (org-todo-keywords '((sequence "TODO(t!)" "|" "CANCELLED(c!)"
                                      "DONEANDDONE(d!)"))))
    (should-error (org-autotask-initialize))))

(ert-deftest org-autotask-org-todo-repeat-to-state ()
  "Test that `org-todo-repeat-to-state' is initialized correctly."
  (org-autotask--test-fixture
      ((org-autotask-keyword-next-action "TODO")
       (org-todo-repeat-to-state nil)
       (org-todo-keywords
        '((sequence "TODO(t!)" "|" "DONE(d!)" "CANCELLED(c!)"))))
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
       (org-autotask-somedaymaybes
        (make-org-autotask-list
         :tag "maybe" :select-char ?d :description "Someday/maybe")))
    (org-autotask-initialize)
    (should (equal org-use-tag-inheritance "may.*"))))

(ert-deftest org-autotask-org-use-tag-inheritance-not-matching-regex ()
  "Test `org-use-tag-inheritance' not matching `org-autotask-somedaymaybe-tag'."
  (org-autotask--test-fixture
      ((org-use-tag-inheritance "foo.*")
       (org-autotask-somedaymaybes
        (make-org-autotask-list
         :tag "maybe" :select-char ?d :description "Someday/maybe")))
    (should-error (org-autotask-initialize))))

(ert-deftest org-autotask-org-use-tag-inheritance-add-to-list ()
  "Test adding `org-autotask-somedaymaybe-tag' to `org-use-tag-inheritance'."
  (org-autotask--test-fixture
      ((org-use-tag-inheritance '("foo" "bar"))
       (org-autotask-somedaymaybes
        (make-org-autotask-list
         :tag "somedaymaybe" :select-char ?d :description "Someday/maybe")))
    (org-autotask-initialize)
    (should (equal org-use-tag-inheritance '("somedaymaybe" "foo" "bar")))))

(ert-deftest org-autotask-org-use-tag-inheritance-already-in-list ()
  "Test `org-use-tag-inheritance' containing `org-autotask-somedaymaybe-tag'."
  (org-autotask--test-fixture
      ((org-use-tag-inheritance '("foo" "bar"))
       (org-autotask-somedaymaybes
        (make-org-autotask-list
         :tag "foo" :select-char ?d :description "Someday/maybe")))
    (should-error (org-autotask-initialize))))

(ert-deftest org-autotask-org-use-tag-inheritance-wrong-type ()
  "Test `org-use-tag-inheritance' being of unrecognized type.."
  (org-autotask--test-fixture
      ((org-use-tag-inheritance 42)
       (org-autotask-somedaymaybes
        (make-org-autotask-list
         :tag "foo" :select-char ?d :description "Someday/maybe")))
    (should-error (org-autotask-initialize))))

(ert-deftest org-autotask-keyword-cancelled-not-in-org-todo-keywords ()
  "Test that the cancelled keyword must be present in `org-todo-keywords'."
  (org-autotask--test-fixture ((org-autotask-keyword-cancelled "ABSENT"))
    (should-error (org-autotask-initialize))))

(ert-deftest org-autotask-keyword-cancelled-without-selection-char-ok ()
  "Test that the cancelled keyword is accepted without the selection char."
  (org-autotask--test-fixture ((org-autotask-keyword-cancelled "CANCELLED"))
    (org-autotask-initialize)))

(ert-deftest org-autotask-keyword-cancelled-absent-but-prefix ()
  "Test that the absent cancelled keyword is diagnosed when it's a prefix."
  (org-autotask--test-fixture
      ((org-autotask-keyword-cancelled "KILL")
       (org-todo-keywords '((sequence "TODO(t!)" "|" "DONE(d!)" "KILLED(k!)"))))
    (should-error (org-autotask-initialize))))

(ert-deftest org-autotask-initialize-org-enforce-todo-dependencies ()
  "Test that `org-enforce-todo-dependencies' is initialized correctly."
  (org-autotask--test-fixture
      ((org-enforce-todo-dependencies nil))
    (org-autotask-initialize)
    (should (equal org-enforce-todo-dependencies t))))

(ert-deftest org-autotask-org-gcal-cancelled-todo-keyword ()
  "Test that `org-gcal-cancelled-todo-keyword' is initialized correctly."
  (org-autotask--test-fixture
      ((org-autotask-keyword-cancelled "KILL")
       (org-todo-keywords '((sequence "TODO" "|" "DONE" "KILL")))
       (org-gcal-cancelled-todo-keyword nil))
    (org-autotask-initialize)
    (should (equal org-gcal-cancelled-todo-keyword
                   org-autotask-keyword-cancelled))))

(ert-deftest org-autotask-initialize-org-stuck-projects ()
  "Test that `org-stuck-projects' is properly initialized."
  (org-autotask--test-fixture
      ((org-autotask-projects
        (make-org-autotask-list
         :tag "prj" :select-char ?p :description "Projects"))
       (org-autotask-somedaymaybes
        (make-org-autotask-list
         :tag "someday" :select-char ?s :description "Someday/Maybe"))
       (org-autotask-keyword-next-action "NEXT")
       (org-todo-keywords
        '((sequence "NEXT(n!)" "|" "DONE(d!)" "CANCELLED(c!)"))))
    (org-autotask-initialize)
    (should (equal org-stuck-projects
                   '("+prj-someday/!NEXT" ("NEXT") nil "")))))

;; Test `org-autotask-agenda-block'

(ert-deftest org-autotask-agenda-block-one-list ()
  "Test for `org-autotask-agenda-block' with one list."
  (org-autotask--test-fixture
      ((gtd-list (make-org-autotask-list :tag "ctx" :select-char ?c
                                         :description "ctx description"))
       (org-autotask-somedaymaybes
        (make-org-autotask-list
         :tag "oneday" :select-char ?d :description "Someday/maybe"))
       (org-autotask-keyword-next-action "DOIT")
       (org-todo-keywords
        '((sequence "DOIT(t!)" "|" "DONE(d!)" "CANCELLED(c!)"))))
    (org-autotask-initialize)
    (should (equal (org-autotask-agenda-block gtd-list)
                   '(tags-todo
                     "ctx-oneday/!DOIT"
                     ((org-agenda-overriding-header "ctx description")
                      (org-agenda-dim-blocked-tasks 'invisible)))))))

(ert-deftest org-autotask-active-todo-search-two-lists ()
  "Test for `org-autotask-agenda-block' with two lists."
  (org-autotask--test-fixture
      ((gtd-list (make-org-autotask-list :tag "ctx" :select-char ?c
                                         :description "ctx description"))
       (gtd-list2 (make-org-autotask-list :tag "foo" :select-char ?f
                                          :description "foo description"))
       (org-autotask-somedaymaybes
        (make-org-autotask-list :tag "maybe" :select-char ?m
                                :description "Someday/maybe"))
       (org-autotask-keyword-next-action "DOIT")
       (org-todo-keywords
        '((sequence "DOIT(t!)" "|" "DONE(d!)" "CANCELLED(c!)"))))
    (org-autotask-initialize)
    (should (equal (org-autotask-agenda-block (list gtd-list gtd-list2)
                                              "Two contexts description")
                   '(tags-todo
                     "ctx-maybe|foo-maybe/!DOIT"
                     ((org-agenda-overriding-header "Two contexts description")
                      (org-agenda-dim-blocked-tasks 'invisible)))))))

(ert-deftest org-autotask-agenda-basic ()
  "Basic test for `org-autotask-agenda'."
  (org-autotask--test-fixture
      ((gtd-list (make-org-autotask-list :tag "foo" :select-char ?f
                                         :description "Foo description"))
       (org-autotask-somedaymaybes
        (make-org-autotask-list :tag "maybe" :select-char ?m
                                :description "Someday/maybe"))
       (org-autotask-keyword-next-action "DOIT"))
    (should (equal (org-autotask-agenda gtd-list)
                   '("Foo description" tags-todo "foo-maybe/!DOIT")))))

(ert-deftest org-autotask-agenda-somedaymaybe-basic ()
  "Basic test for `org-autotask-agenda-somedaymaybe'."
  (org-autotask--test-fixture
      ((org-autotask-somedaymaybes
        (make-org-autotask-list :tag "bar" :select-char ?b
                                :description "Bar description")))
    (should (equal (org-autotask-agenda-somedaymaybe)
                   '("Bar description" tags-todo "bar+LEVEL=2"
                     ((org-agenda-dim-blocked-tasks nil)))))))

(ert-deftest org-autotask-active-non-project-tasks-basic ()
  "Basic test for `org-autotask-agenda-active-non-project-tasks'."
  (org-autotask--test-fixture
      ((org-autotask-projects
        (make-org-autotask-list :tag "prj" :select-char ?p
                                :description "Prj description"))
       (org-autotask-somedaymaybes
        (make-org-autotask-list :tag "maybe" :select-char ?m
                                :description "Maybe description"))
       (org-autotask-waitingfor
        (make-org-autotask-list :tag "wait" :select-char ?w
                                :description "Wait context"))
       (org-autotask-keyword-next-action "NEXT"))
    (should (equal (org-autotask-agenda-active-non-project-tasks)
                   '("Non-project next actions" tags-todo
                     "-prj-wait-maybe/!NEXT"
                     ((org-use-tag-inheritance '("prj" "maybe"))))))))

(ert-deftest org-autotask-agenda-archivable-tasks-basic ()
  "Basic test for `org-autotask-agenda-archivable-tasks'."
  (org-autotask--test-fixture
      ((org-autotask-projects
        (make-org-autotask-list :tag "prj" :select-char ?p
                                :description "Prj description"))
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
       (org-autotask-waitingfor
        (make-org-autotask-list :tag "@wait" :select-char ?t
                                :description "Waiting for"))
       (org-autotask-projects
        (make-org-autotask-list :tag "prj" :select-char ?p
                                :description "Projects"))
       (org-autotask-somedaymaybes
        (make-org-autotask-list :tag "someday" :select-char ?s
                                :description "Someday/Maybe")))
    (should (equal (org-autotask-agenda-contextless-tasks)
                   '(todo
                     "-@home-@work-@wait-prj-someday"
                     ((org-agenda-overriding-header "Contextless tasks")))))))

;; Test creating and completing tasks

(defmacro org-autotask--buffer-test (varlist &rest body)
  "Set up a temp `org' buffer, bind VARLIST and execute BODY in the fixture."
  (declare (indent 1) (debug t))
  `(org-autotask--test-fixture ,varlist
     (with-temp-buffer
       (org-mode)
       ,@body)))

(ert-deftest org-autotask-insert-waiting-for-next-action-basic ()
  "Basic test for `org-autotask-insert-waiting-for-next-action'."
  (org-autotask--buffer-test ()
    (org-insert-todo-heading-respect-content)
    (org-autotask-insert-waiting-for-next-action "Test title")
    (should (string= (org-get-heading t t) "Test title"))
    (should (string= (org-get-todo-state) org-autotask-keyword-next-action))
    (should (equal (org-get-tags) (list (org-autotask-list-tag
                                         org-autotask-waitingfor))))))

(ert-deftest org-autotask-insert-waiting-for-next-action-reject-empty ()
  "Test that `org-autotask-insert-waiting-for-next-action' rejects empty title."
  (org-autotask--buffer-test ()
    (org-insert-todo-heading-respect-content)
    (should-error (org-autotask-insert-waiting-for-next-action ""))))

(ert-deftest org-autotask-insert-waiting-for-next-action-custom-state-tag ()
  "Test `org-autotask-insert-waiting-for-next-action' with non-default config."
  (org-autotask--buffer-test
      ((org-autotask-keyword-next-action "NEXT")
       (org-autotask-waitingfor
        (make-org-autotask-list
         :tag "@wait" :select-char ?f :description "Waiting-for context"))
       (org-todo-keywords
        '((sequence "NEXT(n!)" "|" "DONE(d!)" "CANCELLED(c!)"))))
    (org-autotask-initialize)
    (org-insert-todo-heading-respect-content)
    (org-autotask-insert-waiting-for-next-action "Title text")
    (should (string= (org-get-heading t t) "Title text"))
    (should (string= (org-get-todo-state) org-autotask-keyword-next-action))
    (should (equal (org-get-tags) (list (org-autotask-list-tag
                                         org-autotask-waitingfor))))))

(ert-deftest org-autotask-insert-project-basic ()
  "Basic test for `org-autotask-insert-project'."
  (org-autotask--buffer-test ()
    (org-insert-todo-heading-respect-content)
    (org-autotask-insert-project "Test title")
    (should (string= (org-get-heading t t) "Test title"))
    (should (string= (org-get-todo-state) org-autotask-keyword-next-action))
    (should (equal (org-get-tags) (list (org-autotask-list-tag
                                         org-autotask-projects))))))

(ert-deftest org-autotask-insert-project-reject-empty ()
  "Test that `org-autotask-insert-project' rejects empty title."
  (org-autotask--buffer-test ()
    (org-insert-todo-heading-respect-content)
    (should-error (org-autotask-insert-project ""))))

(ert-deftest org-autotask-insert-project-custom-state-tag ()
  "Test `org-autotask-insert-project' with non-default config."
  (org-autotask--buffer-test
      ((org-autotask-keyword-next-action "FOO")
       (org-autotask-projects
        (make-org-autotask-list
         :tag "bar" :select-char ?b :description "Bars"))
       (org-todo-keywords '((sequence "FOO" "|" "DONE" "CANCELLED"))))
    (org-autotask-initialize)
    (org-insert-todo-heading-respect-content)
    (org-autotask-insert-project "Title text")
    (should (string= (org-get-heading t t) "Title text"))
    (should (string= (org-get-todo-state) org-autotask-keyword-next-action))
    (should (equal (org-get-tags) (list (org-autotask-list-tag
                                         org-autotask-projects))))))

(ert-deftest org-autotask-complete-item-basic ()
  "Basic test for `my-org-complete-item'."
  (org-autotask--buffer-test ()
    (org-insert-todo-heading-respect-content)
    (org-autotask-insert-waiting-for-next-action "Test title")
    (should (string= (org-get-todo-state) org-autotask-keyword-next-action))
    (org-autotask-complete-item)
    (should (string= (org-get-todo-state) org-autotask-keyword-done))
    (should (string= (org-get-heading t t) "Test title"))
    (should (equal (org-get-tags) (list (org-autotask-list-tag
                                         org-autotask-waitingfor))))))

;; Test clock-in automation

(ert-deftest org-autotask-clock-in-actions-basic ()
  "Basic test for `org-autotask--clock-in-actions' with mock actions."
  (org-autotask--buffer-test
      ((actions '()))
    (let* ((action-fn (lambda (x) (push x actions)))
           (org-autotask-clock-in-actions
            `((:property "URL" :action ,action-fn)
              (:property "APP" :action ,action-fn)
              (:property "SHELL" :action ,action-fn)
              (:property "VISIT" :action ,action-fn)
              (:property "EVAL" :action ,action-fn))))
      (org-autotask-initialize)
      (org-insert-todo-heading-respect-content)
      (org-set-property "URL" "http://example.com")
      (org-set-property "APP" "TestApp")
      (org-set-property "SHELL" "echo test")
      (org-set-property "VISIT" "/tmp/test.txt")
      (org-set-property "EVAL" "(message \"test\")")
      (org-clock-in)
      (org-clock-out)
      (should (equal (reverse actions)
                     '("http://example.com" "TestApp" "echo test"
                       "/tmp/test.txt" "(message \"test\")"))))))

(ert-deftest org-autotask-clock-in-actions-multi-value ()
  "Test `org-autotask--clock-in-actions' with multi-value properties."
  (org-autotask--buffer-test
   ((actions '()))
   (let* ((action-fn (lambda (x) (push x actions)))
          (org-autotask-clock-in-actions
           `((:property "URL" :action ,action-fn :multi t))))
     (org-autotask-initialize)
     (org-insert-todo-heading-respect-content)
     (org-set-property "URL" "http://1.example.com")
     (org-entry-add-to-multivalued-property (point) "URL"
                                            "http://2.example.com")
     (org-clock-in)
     (org-clock-out)
     (should (equal (reverse actions)
                    '("http://1.example.com" "http://2.example.com"))))))

(ert-deftest org-autotask-clock-in-actions-default-url ()
  "Test `org-autotask-clock-in-actions' default URL action handler."
  (org-autotask--buffer-test
      ((url-calls '()))
    (cl-letf (((symbol-function 'browse-url)
               (lambda (url) (push url url-calls))))
      (org-autotask-initialize)
      (org-insert-todo-heading-respect-content)
      (org-set-property "URL" "http://example.com")
      (org-clock-in)
      (org-clock-out)
      (should (equal url-calls '("http://example.com"))))))

(ert-deftest org-autotask-clock-in-actions-default-app ()
  "Test `org-autotask-clock-in-actions' default APP action handler."
  (org-autotask--buffer-test
      ((shell-commands '()))
    (cl-letf (((symbol-function 'shell-command)
               (lambda (cmd) (push cmd shell-commands))))
      (org-autotask-initialize)
      (org-insert-todo-heading-respect-content)
      (org-set-property "APP" "TestApp")
      (if (eq system-type 'darwin)
          (progn
            (org-clock-in)
            (should (equal shell-commands '("open -a TestApp"))))
        (progn
          (should-error (org-clock-in))
          (should (equal shell-commands '()))))
      (org-clock-out))))

(ert-deftest org-autotask-clock-in-actions-default-shell ()
  "Test `org-autotask-clock-in-actions' default SHELL action handler."
  (org-autotask--buffer-test
      ((shell-commands '()))
    (cl-letf (((symbol-function 'shell-command)
               (lambda (cmd) (push cmd shell-commands))))
      (org-autotask-initialize)
      (org-insert-todo-heading-respect-content)
      (org-set-property "SHELL" "shell with args")
      (org-clock-in)
      (org-clock-out)
      (should (equal shell-commands '("shell with args"))))))

;; TODO(laurynas): it should be possible to test `find-file' calls directly, but
;; such test does not appear to work.
;; TODO(laurynas): buffer position is not tested
(ert-deftest org-autotask-clock-in-actions-default-visit ()
  "Test `org-autotask-clock-in-actions' default VISIT action handler."
  (org-autotask--buffer-test
      ((find-file-calls '()))
    (cl-letf (((symbol-function 'find-file)
               (lambda (cmd) (push cmd find-file-calls))))
      (org-autotask-initialize)
      (org-insert-todo-heading-respect-content)
      (org-set-property "VISIT" "/tmp/path")
      (org-clock-in)
      (org-clock-out)
      (should (equal find-file-calls '("/tmp/path"))))))

;; TODO(laurynas): add a test `org-autotask-clock-in-actions-default-eval' to test
;; EVAL action. It should be possible to test `eval' calls directly, but such
;; test does not appear to work, and mocking `eval' has too many side effects.

(ert-deftest org-autotask-require-clock-on ()
  "Test that `org-autotask-require-clock-on' does nothing with an active clock."
  (org-autotask--buffer-test ()
    (org-insert-todo-heading-respect-content)
    (org-clock-in)
    (org-autotask-require-org-clock)
    (org-clock-out)))

(ert-deftest org-autotask-require-clock-off ()
  "Test that `org-autotask-require-clock-on' errors out without an active clock."
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
  "Test `org-autotask-clock-gated-commands' blocking commands when not clocking."
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
    (org-autotask-initialize)
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

(ert-deftest org-autotask-with-url-basic ()
  "Basic test for `org-autotask-with-org-node-with-url'."
  (let ((temp-file (make-temp-file "org-tst" nil ".org")))
    (org-autotask--buffer-test
        ((org-agenda-files (list temp-file)))
      (unwind-protect
          (progn
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
              (should executed)))
        (delete-file temp-file)))))

(ert-deftest org-autotask-with-url-not-found ()
  "Test for `org-autotask-with-org-node-with-url' when URL is not found."
  (let ((temp-file (make-temp-file "org-tst" nil ".org")))
    (org-autotask--buffer-test
        ((org-agenda-files (list temp-file)))
      (unwind-protect
          (progn
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
                              "https://3.example.com")))
        (delete-file temp-file)))))

(ert-deftest org-autotask-with-url-multiple-files ()
  "Test `org-autotask-with-org-node-with-url' across multiple files."
  (let ((temp-file-1 (make-temp-file "org-tst" nil ".org"))
        (temp-file-2 (make-temp-file "org-tst" nil ".org")))
    (org-autotask--buffer-test
        ((org-agenda-files (list temp-file-1 temp-file-2)))
      (unwind-protect
          (progn
            (with-temp-file temp-file-1
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
              (should executed)))
        (delete-file temp-file-1)
        (delete-file temp-file-2)))))

(ert-deftest org-autotask-clock-in-node-with-url-basic ()
  "Basic test for `org-autotask-clock-in-node-with-url'."
  (let ((temp-file (make-temp-file "org-tst" nil ".org")))
    (org-autotask--buffer-test
        ((org-agenda-files (list temp-file)))
      (unwind-protect
          (progn
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
            (should (string= (org-get-heading t t) "Item 1")))
        (delete-file temp-file)))))

(ert-deftest org-autotask-with-different-org-clock-no-current-clock ()
  "Test `org-autotask-with-different-org-clock' with no current clock."
  (org-autotask--buffer-test ()
    (org-insert-todo-heading-respect-content)
    (insert "Item 0")
    (org-insert-todo-heading-respect-content)
    (insert "Item 1")
    (let ((item1-pos (save-excursion
                       (org-back-to-heading)
                       (point)))
          executed)
      (when (org-clocking-p)
        (org-clock-out))
      (org-autotask-with-different-org-clock
        (setq executed t)
        (should (org-clocking-p))
        (should (= (save-excursion
                     (goto-char (marker-position org-clock-marker))
                     (org-back-to-heading)
                     (point)) item1-pos)))
      (should executed)
      (should-not (org-clocking-p)))))

(ert-deftest org-autotask-with-different-org-clock-with-existing-clock ()
  "Test `org-autotask-with-different-org-clock' with an existing clock."
  (org-autotask--buffer-test ()
    (org-insert-todo-heading-respect-content)
    (insert "Item 0")
    (org-insert-todo-heading-respect-content)
    (insert "Item 1")
    (let ((item0-pos (point-min))
          ;; TODO(laurynas): rewrite to use markers instead
          item1-pos executed)
      (goto-char item0-pos)
      (org-clock-in)
      (outline-next-heading)
      (org-autotask-with-different-org-clock
        (setq item1-pos (save-excursion
                          (goto-char item0-pos)
                          (outline-next-heading)
                          (point)))
        (setq executed t)
        (should (org-clocking-p))
        (should (= (save-excursion
                     (goto-char (marker-position org-clock-marker))
                     (org-back-to-heading)
                     (point)) item1-pos)))
      (should executed)
      (should (org-clocking-p))
      (should (= (save-excursion
                   (goto-char (marker-position org-clock-marker))
                   (org-back-to-heading)
                   (point)) item0-pos))
      (org-clock-out))))

(ert-deftest org-autotask-with-different-org-clock-error-exit ()
  "Test `org-autotask-with-different-org-clock' cleaning up on error exit."
  (org-autotask--buffer-test ()
    (org-insert-todo-heading-respect-content)
    (insert "Item 0")
    (org-insert-todo-heading-respect-content)
    (insert "Item 1")
    (let ((item0-pos (point-min)))
      (goto-char item0-pos)
      (org-clock-in)
      (outline-next-heading)
      (should-not (= item0-pos (point)))
      (should-error
       (org-autotask-with-different-org-clock
         (user-error "Test error")))
      (should (org-clocking-p))
      (should (= (save-excursion
                   (goto-char (marker-position org-clock-marker))
                   (org-back-to-heading)
                   (point)) item0-pos)))
    (org-clock-out)))

;; TODO(laurynas): idempotency
;; TODO(laurynas): uniqueness in tags
;; TODO(laurynas): uniqueness in keys
;; TODO(laurynas): uniqueness between contexts and waitingfor

(provide 'org-autotask-test)
;;; org-autotask-test.el ends here
