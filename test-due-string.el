;;; test-due-string.el --- Regression tests for TODOIST_DUE_STRING edits -*- lexical-binding: t; -*-

;; Usage: emacs --batch -L . -l ert -l test-due-string.el -f ert-run-tests-batch-and-exit
;;
;; Covers write-back of a hand-edited TODOIST_DUE_STRING property.  The
;; property used to be a one-way mirror of Todoist's recurrence string:
;; `orgist-diff-element' never compared it, so editing it pushed nothing,
;; and a simultaneous reschedule re-applied the *stored* (stale) string —
;; reverting both the recurrence and the date.  See `orgist-diff-element'
;; (:due-string in its field list) and the :due / :due-string cases in
;; `orgist-changes-to-commands'.

(add-to-list 'load-path
             (expand-file-name "~/.emacs.d/elpaca/builds/request"))
;; org-sync-confirm is required by orgist-confirm; prefer the elpaca build,
;; fall back to the source checkout on a fresh clone.
(dolist (dir '("~/.emacs.d/elpaca/builds/org-sync-confirm"
               "~/.emacs.d/elpaca/sources/org-sync-confirm"))
  (when (file-directory-p (expand-file-name dir))
    (add-to-list 'load-path (expand-file-name dir))))
(add-to-list 'load-path default-directory)
(require 'orgist)

(defun orgist-test--due-command (diff snapshot)
  "Run DIFF for a single task with SNAPSHOT and return its item_update due arg.
SNAPSHOT is the plist stored in `orgist-snapshots'; DIFF is the
\(FIELD . (OLD . NEW)) alist `orgist-diff-element' would produce."
  (let ((orgist-snapshots (make-hash-table :test 'equal)))
    (puthash "TESTID" snapshot orgist-snapshots)
    (let* ((cmds (orgist-changes-to-commands (list (cons "TESTID" diff))))
           (upd (seq-find (lambda (c) (equal (alist-get 'type c) "item_update"))
                          cmds)))
      (alist-get 'due (alist-get 'args upd)))))

(defconst orgist-test--snapshot
  '(:due "<2026-06-18 Thu ++1y>" :due-string "every june 18"
    :section-p nil :order 56)
  "Snapshot of a yearly recurring task anchored on June 18.")

(ert-deftest orgist-due-string-pushed-with-reschedule ()
  "Editing TODOIST_DUE_STRING while also moving SCHEDULED pushes the new string."
  (let ((due (orgist-test--due-command
              (list (cons :due (cons "<2026-06-18 Thu ++1y>"
                                     "<2026-06-01 Mon ++1y>"))
                    (cons :due-string (cons "every june 18" "every june 1")))
              orgist-test--snapshot)))
    (should (equal (alist-get 'string due) "every june 1"))
    (should (equal (alist-get 'date due) "2026-06-01"))
    (should (eq (alist-get 'is_recurring due) t))))

(ert-deftest orgist-due-string-pushed-alone ()
  "Editing only TODOIST_DUE_STRING pushes the new string on the unchanged date."
  (let ((due (orgist-test--due-command
              (list (cons :due-string (cons "every june 18" "every june 1")))
              orgist-test--snapshot)))
    (should (equal (alist-get 'string due) "every june 1"))
    ;; SCHEDULED was not touched, so the date stays put and Todoist re-anchors.
    (should (equal (alist-get 'date due) "2026-06-18"))))

(ert-deftest orgist-due-string-anchor-preserved-on-reschedule ()
  "Moving SCHEDULED without touching the string preserves the stored anchor."
  (let ((due (orgist-test--due-command
              (list (cons :due (cons "<2026-06-18 Thu ++1y>"
                                     "<2026-06-01 Mon ++1y>")))
              orgist-test--snapshot)))
    (should (equal (alist-get 'string due) "every june 18"))
    (should (equal (alist-get 'date due) "2026-06-01"))))

(provide 'test-due-string)
;;; test-due-string.el ends here
