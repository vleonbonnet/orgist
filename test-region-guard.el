;;; test-region-guard.el --- Regression tests for pull-side org command guards -*- lexical-binding: t; -*-

;; Usage: emacs --batch -L . -l ert -l test-region-guard.el -f ert-run-tests-batch-and-exit
;;
;; Locks in the guards added after the 2026-08-16 data-corruption
;; incident: `org-todo'/`org-schedule'/`org-deadline' loop over every
;; headline in the active region (default since Org 9.4), so an
;; auto-pull updating one element while the user had a region active
;; smeared that element's state and dates across ~90 headings.  Also
;; covers the org-blocker-hook guard (a completed Todoist parent with
;; open subtasks must still become DONE), preservation of user-chosen
;; done-type keywords (CANCELED), and the no-op state transition skip
;; that prevents org-auto-repeat bounces on repeatered DONE tasks.

(add-to-list 'load-path
             (expand-file-name "~/.emacs.d/elpaca/builds/request"))
(add-to-list 'load-path default-directory)
(require 'ert)
(require 'orgist)

(defmacro orgist-test--with-org-buffer (content &rest body)
  "Run BODY in a temp org buffer containing CONTENT, point at start."
  (declare (indent 1))
  `(with-temp-buffer
     (let ((org-todo-keywords '((sequence "TODO" "|" "DONE" "CANCELED"))))
       (insert ,content)
       (org-mode)
       (goto-char (point-min))
       ,@body
       ;; Leave no modified file-less buffer behind
       (set-buffer-modified-p nil))))

(defun orgist-test--element (&rest overrides)
  "Build a minimal Todoist element alist, applying OVERRIDES."
  (let ((base '((id . "TARGET1") (content . "Target task")
                (checked . :json-false) (priority . 1)
                (project_id . "PROJX"))))
    (dolist (kv overrides base)
      (setf (alist-get (car kv) base) (cdr kv)))))

(defun orgist-test--goto-id (id)
  "Move point to the heading with ID."
  (goto-char (point-min))
  (re-search-forward (format "^:ID: +%s$" (regexp-quote id)))
  (org-back-to-heading t))

(ert-deftest orgist-region-guard/no-smear-across-active-region ()
  "An active region must not spread one element's dates and state."
  (orgist-test--with-org-buffer
      "* TODO Bystander one
SCHEDULED: <2026-01-03 Sat ++1w>
:PROPERTIES:
:ID:       BYSTANDER1
:END:
* DONE Bystander two
:PROPERTIES:
:ID:       BYSTANDER2
:END:
* TODO Target task
:PROPERTIES:
:ID:       TARGET1
:END:
"
    ;; Activate a region from the top of the buffer to the target — the
    ;; incident's geometry: user's mark near the top, point left on the
    ;; element being updated, bystanders inside the region.
    (transient-mark-mode 1)
    (orgist-test--goto-id "TARGET1")
    (push-mark (point-min) t t)
    (let ((mark-active t))
      (orgist-update-element
       (orgist-test--element '(checked . t)
                             '(due . ((date . "2026-08-15") (string . "Aug 15")))
                             '(deadline . ((date . "2026-08-31"))))
       t))
    (orgist-test--goto-id "TARGET1")
    (should (equal "DONE" (org-get-todo-state)))
    (should (org-entry-get (point) "DEADLINE"))
    (orgist-test--goto-id "BYSTANDER1")
    (should (equal "TODO" (org-get-todo-state)))
    (should-not (org-entry-get (point) "DEADLINE"))
    (should (equal "<2026-01-03 Sat ++1w>" (org-entry-get (point) "SCHEDULED")))
    (orgist-test--goto-id "BYSTANDER2")
    (should (equal "DONE" (org-get-todo-state)))
    (should-not (org-entry-get (point) "DEADLINE"))
    (should-not (org-entry-get (point) "SCHEDULED"))))

(ert-deftest orgist-region-guard/done-applies-despite-todo-dependencies ()
  "A parent completed in Todoist becomes DONE even with open children."
  (orgist-test--with-org-buffer
      "* TODO Target task
:PROPERTIES:
:ID:       TARGET1
:END:
** TODO Open child
:PROPERTIES:
:ID:       CHILD1
:END:
"
    (let ((org-enforce-todo-dependencies t)
          (org-blocker-hook '(org-block-todo-from-children-or-siblings-or-parent)))
      (orgist-test--goto-id "TARGET1")
      (orgist-update-element (orgist-test--element '(checked . t)) t))
    (orgist-test--goto-id "TARGET1")
    (should (equal "DONE" (org-get-todo-state)))))

(ert-deftest orgist-region-guard/done-keyword-preserved ()
  "A user-chosen done-type keyword survives a checked pull."
  (orgist-test--with-org-buffer
      "* CANCELED Target task
:PROPERTIES:
:ID:       TARGET1
:END:
"
    (orgist-test--goto-id "TARGET1")
    (orgist-update-element (orgist-test--element '(checked . t)) t)
    (orgist-test--goto-id "TARGET1")
    (should (equal "CANCELED" (org-get-todo-state)))))

(ert-deftest orgist-region-guard/no-repeat-bounce-on-unchanged-done ()
  "Re-applying DONE to a repeatered DONE task must not bounce it to TODO."
  (orgist-test--with-org-buffer
      "* DONE Target task
SCHEDULED: <2026-01-05 Mon ++1w>
:PROPERTIES:
:ID:       TARGET1
:END:
"
    (orgist-test--goto-id "TARGET1")
    (orgist-update-element
     (orgist-test--element '(checked . t)
                           '(due . ((date . "2026-01-05") (string . "Jan 5"))))
     t)
    (orgist-test--goto-id "TARGET1")
    (should (equal "DONE" (org-get-todo-state)))))

;;; test-region-guard.el ends here
