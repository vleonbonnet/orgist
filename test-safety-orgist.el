;;; test-safety-orgist.el --- Orgist's use of the safety net -*- lexical-binding: t; -*-

;; Usage: emacs --batch -L . -l ert -l test-safety-orgist.el -f ert-run-tests-batch-and-exit
;;
;; How orgist applies a pull under org-sync-safety: removed text goes
;; to the journal and removed files to the trash; a task update that
;; changes text outside its own subtree, or a pull that loses an
;; element Todoist did not delete, rolls every project buffer back.
;; Also: deleting a Todoist sub-project removes only its heading (it
;; used to delete the whole parent project file), reset sets files
;; aside instead of deleting them, and journal entries can be put back.

(add-to-list 'load-path
             (expand-file-name "~/.emacs.d/elpaca/builds/request"))
(dolist (dir '("~/.emacs.d/elpaca/builds/org-sync-confirm"
               "~/.emacs.d/elpaca/sources/org-sync-confirm"))
  (when (file-directory-p (expand-file-name dir))
    (add-to-list 'load-path (expand-file-name dir))))
(add-to-list 'load-path default-directory)
(require 'test-isolation)
(require 'ert)
(require 'cl-lib)
(require 'orgist)

(defconst orgist-test--proj
  ":PROPERTIES:
:ID:       PROJ1
:TODOIST-PROJECT:
:END:
#+TITLE: Proj
* TODO Keep
:PROPERTIES:
:ID:       T1
:TODOIST-ORDER: 0
:END:

* TODO Doomed
:PROPERTIES:
:ID:       T2
:TODOIST-ORDER: 1
:END:

Doomed description.

** Notes
Only in org.

* Sub
:PROPERTIES:
:ID:       SUB1
:END:

** TODO Sub task
:PROPERTIES:
:ID:       T3
:TODOIST-ORDER: 0
:END:
")

(defconst orgist-test--other
  ":PROPERTIES:
:ID:       PROJ2
:TODOIST-PROJECT:
:END:
#+TITLE: Other
* TODO Elsewhere
:PROPERTIES:
:ID:       T4
:TODOIST-ORDER: 0
:END:
")

(defmacro orgist-test--with-project (&rest body)
  "Run BODY with a throwaway base dir holding Proj.org and Other.org.
DIR is bound to the directory.  Saves are deferred, as during a pull."
  (declare (indent 0))
  `(let* ((dir (file-name-as-directory (make-temp-file "orgist-safety-" t)))
          (orgist-base-dir dir)
          (orgist-snapshot-file (expand-file-name "snapshots.el" dir))
          (orgist-sync-token-filename (expand-file-name "sync_token" dir))
          (orgist-labels-file (expand-file-name "labels.el" dir))
          (orgist-log-file nil)
          (orgist-history-directory nil)
          (orgist--history-cache nil)
          (orgist-snapshots (make-hash-table :test 'equal))
          (orgist--snapshot-count-on-disk nil)
          (orgist-reminders nil)
          (orgist-labels nil)
          (orgist-sync-comments nil)
          (orgist-sync-attachments nil)
          (orgist-enable-write-back t)
          (orgist-project-buffer-cache nil)
          (orgist--batch-save-pending (make-hash-table :test 'eq)))
     (with-temp-file (expand-file-name "Proj.org" dir) (insert orgist-test--proj))
     (with-temp-file (expand-file-name "Other.org" dir) (insert orgist-test--other))
     (unwind-protect
         (progn ,@body)
       (dolist (buf (buffer-list))
         (when (and (buffer-file-name buf)
                    (string-prefix-p (expand-file-name dir) (expand-file-name (buffer-file-name buf))))
           (with-current-buffer buf (set-buffer-modified-p nil))
           (kill-buffer buf)))
       (delete-directory dir t))))

(defun orgist-test--buffer (name)
  (find-file-noselect (expand-file-name name orgist-base-dir)))

(defun orgist-test--text (name)
  (with-current-buffer (orgist-test--buffer name) (buffer-string)))

(defun orgist-test--journal ()
  (let ((file (orgist--journal-file)))
    (when (file-exists-p file)
      (with-temp-buffer (insert-file-contents file) (buffer-string)))))

(defun orgist-test--task (id content &rest more)
  "A Todoist item alist for task ID with CONTENT in project PROJ1."
  (append more
          `((id . ,id) (content . ,content) (description . "")
            (project_id . "PROJ1") (section_id . nil) (parent_id . nil)
            (checked . nil) (priority . 1) (labels . []) (child_order . 0)
            (added_at . "2026-01-01T10:00:00Z"))))

(ert-deftest orgist-safety/remote-task-deletion-is-journaled ()
  "A task deleted in Todoist leaves the file, but its text is in the journal."
  (orgist-test--with-project
    (let ((before (orgist-test--text "Proj.org")))
      (orgist--apply-pull nil nil '(((id . "T2") (content . "Doomed") (is_deleted . t)
                                     (project_id . "2222222222222222"))))
      (should-not (string-match-p "Doomed\\|Only in org" (orgist-test--text "Proj.org")))
      (let ((journal (orgist-test--journal)))
        (should (string-match-p "Deleted in Todoist" journal))
        (should (string-match-p ":ELEMENT: T2" journal))
        ;; Verbatim, Org syntax escaped inside the src block.
        (should (string-match-p ",\\*\\* Notes\nOnly in org\\." journal)))
      (should-not (equal before (orgist-test--text "Proj.org"))))))

(ert-deftest orgist-safety/subproject-deletion-keeps-the-parent-file ()
  "Deleting a sub-project removes its heading, never the parent project's file."
  (orgist-test--with-project
    (orgist--apply-pull '(((id . "SUB1") (name . "Sub") (parent_id . "PROJ1") (is_deleted . t)))
                        nil nil)
    (should (file-exists-p (expand-file-name "Proj.org" orgist-base-dir)))
    (let ((text (orgist-test--text "Proj.org")))
      (should (string-match-p "^\\* TODO Keep$" text))
      (should (string-match-p "^\\* TODO Doomed$" text))
      (should-not (string-match-p "^\\* Sub$\\|Sub task" text)))
    (should (string-match-p "Sub-project deleted in Todoist" (orgist-test--journal)))))

(ert-deftest orgist-safety/root-project-deletion-moves-the-file-to-the-trash ()
  "A deleted root project's file is journaled and moved to the trash."
  (orgist-test--with-project
    (orgist-test--buffer "Other.org")
    (orgist--apply-pull '(((id . "PROJ2") (name . "Other") (parent_id . nil) (is_deleted . t)))
                        nil nil)
    (should-not (file-exists-p (expand-file-name "Other.org" orgist-base-dir)))
    (should (directory-files-recursively (orgist--trash-directory) "\\`Other\\.org\\'"))
    (should (string-match-p ":KIND: removed-file" (orgist-test--journal)))))

(ert-deftest orgist-safety/replaced-description-journaled-only-when-changed ()
  "Rebuilding a description to the same text journals nothing; a real change does."
  (skip-unless (executable-find "pandoc"))
  (orgist-test--with-project
    (orgist--apply-pull nil nil (list (orgist-test--task
                                       "T2" "Doomed"
                                       '(description . "Doomed description.\n\n# Notes\n\nOnly in org."))))
    (should-not (orgist-test--journal))
    (orgist--apply-pull nil nil (list (orgist-test--task
                                       "T2" "Doomed"
                                       '(description . "Rewritten in Todoist."))))
    (let ((journal (orgist-test--journal)))
      (should (string-match-p "Replaced description of Doomed" journal))
      (should (string-match-p "Doomed description\\." journal))
      (should (string-match-p ",\\*\\* Notes" journal)))
    (should (string-match-p "Rewritten in Todoist\\." (orgist-test--text "Proj.org")))))

(ert-deftest orgist-safety/update-touching-another-entry-rolls-back ()
  "A task update that changes text outside its subtree aborts and restores the buffers."
  (orgist-test--with-project
    (let ((before (orgist-test--text "Proj.org")))
      (cl-letf (((symbol-function 'orgist-deduplicate-logbook)
                 (lambda (&rest _)
                   (save-excursion (goto-char (point-max)) (insert "* TODO Smeared\n")))))
        (should-error (orgist--apply-pull nil nil (list (orgist-test--task "T1" "Keep renamed")))
                      :type 'org-sync-safety-violation))
      (with-current-buffer (orgist-test--buffer "Proj.org")
        (should (equal (buffer-string) before))
        (should-not (buffer-modified-p))))))

(ert-deftest orgist-safety/unexplained-vanishing-element-rolls-back ()
  "An element lost through an unguarded path is caught at the end of the pull."
  (orgist-test--with-project
    (let ((before (orgist-test--text "Proj.org")))
      (cl-letf (((symbol-function 'orgist-deduplicate-logbook)
                 (lambda (&rest _)
                   ;; Exempt from the region guard, as a hook would be.
                   (let ((org-sync-safety-guard-suspended t))
                     (save-excursion
                       (goto-char (orgist-find-element-by-id "T1"))
                       (orgist-delete-subtree))))))
        (let ((err (should-error (orgist--apply-pull nil nil (list (orgist-test--task "T2" "Doomed")))
                                 :type 'org-sync-safety-violation)))
          (should (string-match-p "T1 vanished from Proj.org" (cadr err)))))
      (should (equal (orgist-test--text "Proj.org") before)))))

(defvar orgist-test--hook-ran nil)

(ert-deftest orgist-safety/user-hooks-may-change-other-entries ()
  "A user's state-change hook completing another task does not abort the pull."
  (orgist-test--with-project
    (let ((org-after-todo-state-change-hook
           (list (lambda ()
                   (setq orgist-test--hook-ran t)
                   (save-excursion
                     (goto-char (orgist-find-element-by-id "T1"))
                     (org-entry-put (point) "TRIGGERED" "yes"))))))
      (setq orgist-test--hook-ran nil)
      (orgist--apply-pull nil nil (list (orgist-test--task "T2" "Doomed" '(checked . t))))
      (should orgist-test--hook-ran)
      (with-current-buffer (orgist-test--buffer "Proj.org")
        (goto-char (orgist-find-element-by-id "T1"))
        (should (equal (org-entry-get (point) "TRIGGERED") "yes"))
        (goto-char (orgist-find-element-by-id "T2"))
        (should (equal (org-get-todo-state) "DONE"))))))

(ert-deftest orgist-safety/rollback-keeps-unsaved-user-edits ()
  "A failed pull gives a buffer back its unsaved text, still unsaved."
  (orgist-test--with-project
    (with-current-buffer (orgist-test--buffer "Proj.org")
      (goto-char (point-max))
      (insert "* TODO Typed but not saved\n"))
    (let ((edited (orgist-test--text "Proj.org")))
      (cl-letf (((symbol-function 'orgist-deduplicate-logbook)
                 (lambda (&rest _) (error "Boom"))))
        (should-error (orgist--apply-pull nil nil (list (orgist-test--task "T1" "Keep")))))
      (with-current-buffer (orgist-test--buffer "Proj.org")
        (should (equal (buffer-string) edited))
        (should (buffer-modified-p))))))

(ert-deftest orgist-safety/history-records-each-state ()
  "Checkpoints record the files only when something changed."
  (skip-unless (executable-find org-sync-safety-git-program))
  (orgist-test--with-project
    (should (orgist--history-checkpoint "First"))
    (should-not (orgist--history-checkpoint "Nothing changed"))
    (with-current-buffer (orgist-test--buffer "Proj.org")
      (goto-char (point-max))
      (insert "* TODO New\n")
      (let ((orgist--batch-save-pending nil)
            (orgist--inhibit-after-save t))
        (save-buffer)))
    (should (orgist--history-checkpoint "Second"))
    (should (equal (mapcar #'caddr (org-sync-safety-history-log (orgist--history) "Proj.org"))
                   '("Second" "First")))
    ;; The history directory is not a project file and is never recorded.
    (should-not (member ".history" (directory-files orgist-base-dir nil "\\.org\\'")))))

(ert-deftest orgist-safety/reset-refuses-foreign-files ()
  "Reset will not touch a directory holding an .org file orgist did not create."
  (orgist-test--with-project
    (with-temp-file (expand-file-name "notes.org" orgist-base-dir) (insert "* My notes\n"))
    (should-error (orgist-reset) :type 'user-error)
    (should (file-exists-p (expand-file-name "notes.org" orgist-base-dir)))))

(ert-deftest orgist-safety/reset-sets-files-aside ()
  "Reset moves orgist's files into the history directory and deletes nothing."
  (orgist-test--with-project
    (with-temp-file orgist-snapshot-file (insert ";; snapshots\nnil\n"))
    (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t))
              ((symbol-function 'orgist) #'ignore))
      (orgist-reset))
    (should-not (directory-files orgist-base-dir nil "\\.org\\'"))
    (let ((aside (car (directory-files (orgist--safety-directory) t "\\`reset-"))))
      (should aside)
      (should (file-exists-p (expand-file-name "Proj.org" aside)))
      (should (file-exists-p (expand-file-name "Other.org" aside)))
      (should (file-exists-p (expand-file-name "snapshots.el" aside))))))

(ert-deftest orgist-safety/journal-entry-restores-without-todoist-binding ()
  "A deleted task comes back under its parent, unbound, so write-back re-creates it."
  (orgist-test--with-project
    (orgist--apply-pull nil nil '(((id . "T3") (content . "Sub task") (is_deleted . t)
                                   (project_id . "2222222222222222"))))
    (with-current-buffer (find-file-noselect (orgist--journal-file))
      (goto-char (point-min))
      (re-search-forward "^\\* .*Deleted Sub task")
      (orgist-journal-restore))
    (with-current-buffer (orgist-test--buffer "Proj.org")
      (goto-char (point-min))
      (re-search-forward "^\\*\\* TODO Sub task$")
      ;; Back under its former parent, without the Todoist ID.
      (should (equal (org-get-outline-path) '("Sub")))
      (should-not (org-entry-get (point) "ID"))
      (should-not (org-entry-get (point) "TODOIST-ORDER")))))

(provide 'test-safety-orgist)
;;; test-safety-orgist.el ends here
