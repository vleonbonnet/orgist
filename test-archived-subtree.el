;;; test-archived-subtree.el --- Regression tests for archived subtrees -*- lexical-binding: t; -*-

;; Usage: emacs --batch -L . -l ert -l test-archived-subtree.el -f ert-run-tests-batch-and-exit
;;
;; A repeating task can keep the record of each past cycle, and the
;; template of the next one, in a subtree tagged :ARCHIVE: under it.
;; Such a subtree is outside the sync: its headings are neither
;; Todoist elements nor part of the task's description.  A task moved
;; into it is deleted in Todoist, a TODO heading written in it is never
;; created there, replacing the task's description keeps it, and a pull
;; that deletes one of its tasks in Todoist leaves the record alone.
;; Archived sections are Todoist elements and keep syncing.

(add-to-list 'load-path
             (expand-file-name "~/.emacs.d/elpaca/builds/request"))
;; org-sync-confirm is required by orgist-confirm; prefer the elpaca build,
;; fall back to the source checkout on a fresh clone.
(dolist (dir '("~/.emacs.d/elpaca/builds/org-sync-confirm"
               "~/.emacs.d/elpaca/sources/org-sync-confirm"))
  (when (file-directory-p (expand-file-name dir))
    (add-to-list 'load-path (expand-file-name dir))))
(add-to-list 'load-path default-directory)
(require 'test-isolation)
(require 'ert)
(require 'orgist)

(defconst orgist-test--project-file
  ":PROPERTIES:
:ID:       PROJ1
:TODOIST-PROJECT:
:END:
#+TITLE: Proj
* TODO Parent
:PROPERTIES:
:ID:       T1
:TODOIST-ORDER: 0
:END:
Parent notes.

** Notes
Some text.

** DONE Step one
:PROPERTIES:
:ID:       C1
:TODOIST-ORDER: 0
:END:

** TODO Step two
:PROPERTIES:
:ID:       C2
:TODOIST-ORDER: 1
:END:
"
  "A project holding a task with a description sub-heading and two subtasks.")

(defmacro orgist-test--with-project (&rest body)
  "Run BODY with a throwaway orgist project directory as `orgist-base-dir'.
The snapshots of the three tasks match their local state, so only
the edits made by BODY can diff.  BODY runs in the buffer visiting
the project file."
  (declare (indent 0))
  `(let* ((dir (make-temp-file "orgist-test-" t))
          (file (expand-file-name "Proj.org" dir))
          (orgist-base-dir dir)
          (orgist-snapshot-file (expand-file-name "snapshots.el" dir))
          (orgist--write-back-stamps nil)
          (orgist--write-back-stamps-path nil)
          (orgist--pending-stamps nil)
          (orgist-snapshots (make-hash-table :test 'equal))
          (orgist-sync-comments nil)
          (orgist-sync-attachments nil)
          (orgist-enable-write-back t)
          (orgist-project-buffer-cache nil))
     (with-temp-file file (insert orgist-test--project-file))
     (let ((buf (find-file-noselect file)))
       (unwind-protect
           (with-current-buffer buf
             (orgist-build-id-cache)
             (dolist (id '("T1" "C1" "C2"))
               (goto-char (orgist-find-element-by-id id))
               (puthash id (orgist-element-local-state) orgist-snapshots))
             ,@body)
         (with-current-buffer buf
           (set-buffer-modified-p nil))
         (kill-buffer buf)
         (delete-directory dir t)))))

(defun orgist-test--archive-step-one ()
  "Move Step one into a new archived subtree at the end of Parent; save."
  (goto-char (orgist-find-element-by-id "C1"))
  (let* ((beg (point))
         (end (orgist--subtree-end))
         (text (buffer-substring beg end)))
    (delete-region beg end)
    (goto-char (point-max))
    (unless (bolp) (insert "\n"))
    (insert "** Cycle 2026 :ARCHIVE:\n"
            (replace-regexp-in-string "^\\*\\* " "*** " text)))
  (save-buffer))

(ert-deftest orgist-archived-subtree/root-needs-own-tag-and-no-section ()
  "Only a heading tagged :ARCHIVE: itself, and not a section, roots an archived subtree."
  (with-temp-buffer
    (org-mode)
    (insert "* TODO Task\n"
            "** Cycle :ARCHIVE:\n"
            "*** DONE Old step\n"
            "* Done section :ARCHIVE:\n"
            ":PROPERTIES:\n:SECTION: t\n:END:\n"
            "** TODO Task in archived section\n")
    (cl-flet ((at (title)
                (goto-char (point-min))
                (re-search-forward (concat "^\\*+ .*" (regexp-quote title)))
                (line-beginning-position)))
      (should (orgist--archived-root-p (at "Cycle")))
      (should-not (orgist--archived-root-p (at "Old step")))
      (should (orgist--in-archived-subtree-p (at "Old step")))
      (should-not (orgist--archived-root-p (at "Done section")))
      (should-not (orgist--in-archived-subtree-p (at "Task in archived section")))
      (should (orgist--element-heading-p (at "Task in archived section")))
      (should-not (orgist--element-heading-p (at "Old step")))
      (should (orgist--element-heading-p (at "TODO Task"))))))

(ert-deftest orgist-archived-subtree/archived-task-is-deleted-only ()
  "Moving a synced subtask into an archived subtree deletes it and changes nothing else."
  (orgist-test--with-project
    (orgist-test--archive-step-one)
    (should-not (orgist-find-element-by-id "C1"))
    (let ((changes (orgist-diff-all-elements)))
      (should (equal changes '(("C1" . deleted))))
      (let ((commands (orgist-changes-to-commands changes)))
        (should (length= commands 1))
        (should (equal (alist-get 'type (car commands)) "item_delete"))))))

(ert-deftest orgist-archived-subtree/template-tasks-are-never-created ()
  "A TODO heading in an archived subtree is not a new task."
  (orgist-test--with-project
    (goto-char (point-max))
    (insert "** Template :ARCHIVE:\n*** TODO Fresh step\n*** TODO Other step\n")
    (save-buffer)
    (should-not (orgist-diff-all-elements))
    (goto-char (point-min))
    (re-search-forward "Fresh step")
    (should-not (org-entry-get (point) orgist-todoist-id-property))))

(ert-deftest orgist-archived-subtree/description-leaves-it-out ()
  "The task's description is the same with or without an archived subtree."
  (orgist-test--with-project
    (goto-char (orgist-find-element-by-id "T1"))
    (let ((before (orgist-extract-body-text)))
      (should (equal before "Parent notes.\n\n* Notes\nSome text."))
      (orgist-test--archive-step-one)
      (goto-char (orgist-find-element-by-id "T1"))
      (should (equal (orgist-extract-body-text) before)))))

(ert-deftest orgist-archived-subtree/replacing-description-keeps-it ()
  "Clearing the task's description removes its notes and keeps the archived subtree."
  (orgist-test--with-project
    (orgist-test--archive-step-one)
    (goto-char (orgist-find-element-by-id "T1"))
    (orgist-clear-body)
    (let ((text (buffer-substring-no-properties (point-min) (point-max))))
      (should-not (string-match-p "Some text" text))
      (should (string-match-p "^\\*\\* Cycle 2026 +:ARCHIVE:$" text))
      (should (string-match-p "^\\*\\*\\* DONE Step one$" text))
      (should (string-match-p "^:ID: +C1$" text)))))

(ert-deftest orgist-archived-subtree/remote-deletion-keeps-the-record ()
  "A pull that deletes an archived task in Todoist leaves its record alone."
  (orgist-test--with-project
    (orgist-test--archive-step-one)
    (orgist-delete-element "C1" "Step one")
    (goto-char (point-min))
    (should (re-search-forward "^\\*\\*\\* DONE Step one$" nil t))))

;;; test-archived-subtree.el ends here
