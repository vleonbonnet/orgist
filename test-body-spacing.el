;;; test-body-spacing.el --- Regression tests for body spacing normalization -*- lexical-binding: t; -*-

;; Usage: emacs --batch -L . -l ert -l test-body-spacing.el -f ert-run-tests-batch-and-exit
;;
;; `orgist--normalize-body-spacing' guarantees one blank line between
;; a task's metadata and its description (README.org § Body Spacing).
;; With `org-log-into-drawer' the state entries live in a :LOGBOOK:
;; drawer, and the description pulled from Todoist was inserted right
;; after the drawer's :END: line — the drawer case was never covered,
;; only bare "- State" lines (seen 2026-09-12).

(add-to-list 'load-path
             (expand-file-name "~/.emacs.d/elpaca/builds/request"))
;; org-sync-confirm is required by orgist-confirm; prefer the elpaca build,
;; fall back to the source checkout on a fresh clone.
(dolist (dir '("~/.emacs.d/elpaca/builds/org-sync-confirm"
               "~/.emacs.d/elpaca/sources/org-sync-confirm"))
  (when (file-directory-p (expand-file-name dir))
    (add-to-list 'load-path (expand-file-name dir))))
(add-to-list 'load-path default-directory)
(require 'ert)
(require 'orgist)

(defun orgist-test--normalize (content)
  "Return CONTENT after normalizing the spacing of its first heading."
  (with-temp-buffer
    (insert content)
    (org-mode)
    (goto-char (point-min))
    (orgist--normalize-body-spacing)
    (prog1 (buffer-substring-no-properties (point-min) (point-max))
      (set-buffer-modified-p nil))))

(defconst orgist-test--spaced
  "* TODO Task
:PROPERTIES:
:ID:       T1
:END:
:LOGBOOK:
- [2026-09-12 Sat 05:06] From            to \"TODO\"
:END:

Description line.

* TODO Next
:PROPERTIES:
:ID:       T2
:END:
"
  "The normalized shape: drawer, one blank line, description, one blank line.")

(ert-deftest orgist-body-spacing/blank-line-after-logbook-drawer ()
  "A description glued to the LOGBOOK :END: line gets one blank line."
  (should (equal (orgist-test--normalize
                  "* TODO Task
:PROPERTIES:
:ID:       T1
:END:
:LOGBOOK:
- [2026-09-12 Sat 05:06] From            to \"TODO\"
:END:
Description line.
* TODO Next
:PROPERTIES:
:ID:       T2
:END:
")
                 orgist-test--spaced)))

(ert-deftest orgist-body-spacing/normalized-shape-is-stable ()
  "Normalizing an already normalized task changes nothing."
  (should (equal (orgist-test--normalize orgist-test--spaced)
                 orgist-test--spaced)))

(ert-deftest orgist-body-spacing/no-blank-line-inside-metadata ()
  "The LOGBOOK drawer stays glued to the property drawer."
  (should (equal (orgist-test--normalize
                  "* TODO Task
:PROPERTIES:
:ID:       T1
:END:

:LOGBOOK:
- [2026-09-12 Sat 05:06] From            to \"TODO\"
:END:
Description line.
* TODO Next
:PROPERTIES:
:ID:       T2
:END:
")
                 orgist-test--spaced)))

(ert-deftest orgist-body-spacing/logbook-only-body-gets-no-extra-line ()
  "A task whose body is only a LOGBOOK drawer keeps a single trailing blank line."
  (should (equal (orgist-test--normalize
                  "* TODO Task
:PROPERTIES:
:ID:       T1
:END:
:LOGBOOK:
- [2026-09-12 Sat 05:06] From            to \"TODO\"
:END:
* TODO Next
:PROPERTIES:
:ID:       T2
:END:
")
                 "* TODO Task
:PROPERTIES:
:ID:       T1
:END:
:LOGBOOK:
- [2026-09-12 Sat 05:06] From            to \"TODO\"
:END:

* TODO Next
:PROPERTIES:
:ID:       T2
:END:
")))

(ert-deftest orgist-body-spacing/pulled-description-after-created-entry ()
  "A pulled task with an added_at and a description ends up correctly spaced."
  (let ((orgist-enable-write-back nil)
        (orgist-sync-comments nil)
        (orgist-sync-attachments nil)
        (orgist-reminders nil)
        (org-log-into-drawer t))
    (with-temp-buffer
      (insert "* TODO Task
:PROPERTIES:
:ID:       T1
:END:
* TODO Next
:PROPERTIES:
:ID:       T2
:END:
")
      (org-mode)
      (goto-char (point-min))
      (orgist-update-element
       '((id . "T1") (content . "Task") (checked . :json-false) (priority . 1)
         (project_id . "P") (added_at . "2026-09-12T12:06:00Z")
         (description . "Description line."))
       t)
      (goto-char (point-min))
      (re-search-forward "^:END:\n:LOGBOOK:\n")
      (re-search-forward "^:END:\n")
      (should (looking-at "\nDescription line\\.\n\n\\* TODO Next"))
      (set-buffer-modified-p nil))))

(provide 'test-body-spacing)
;;; test-body-spacing.el ends here
