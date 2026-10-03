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

(ert-deftest orgist-body-spacing/description-subheading-gets-blank ()
  "A description sub-heading is separated from its own content.
Normalization used to stop at the first following heading, leaving
pandoc-generated sub-headings glued to their paragraphs.  The blank
before the next sibling task is the insertion flow's business, not
the entry normalizer's."
  (should (equal (orgist-test--normalize
                  "* TODO Task
:PROPERTIES:
:ID:       T1
:END:
Description.
** Detail
Sub body.
* TODO Next
:PROPERTIES:
:ID:       T2
:END:
")
                 "* TODO Task
:PROPERTIES:
:ID:       T1
:END:

Description.

** Detail

Sub body.
* TODO Next
:PROPERTIES:
:ID:       T2
:END:
")))

(ert-deftest orgist-body-spacing/bullet-description-gets-blank ()
  "A bullet-list description is content and gets its blank line.
Any leading \"- \" used to be treated as a logbook entry, gluing
the list to the metadata."
  (should (equal (orgist-test--normalize
                  "* TODO Task
:PROPERTIES:
:ID:       T1
:END:
- First item
- Second item
* TODO Next
:PROPERTIES:
:ID:       T2
:END:
")
                 "* TODO Task
:PROPERTIES:
:ID:       T1
:END:

- First item
- Second item

* TODO Next
:PROPERTIES:
:ID:       T2
:END:
")))

(ert-deftest orgist-body-spacing/block-contents-protected ()
  "Blank lines inside literal blocks are data and are never collapsed."
  (should (equal (orgist-test--normalize
                  "* TODO Task
:PROPERTIES:
:ID:       T1
:END:
#+BEGIN_SRC python
x = 1


y = 2
#+END_SRC
* TODO Next
:PROPERTIES:
:ID:       T2
:END:
")
                 "* TODO Task
:PROPERTIES:
:ID:       T1
:END:

#+BEGIN_SRC python
x = 1


y = 2
#+END_SRC

* TODO Next
:PROPERTIES:
:ID:       T2
:END:
")))

(ert-deftest orgist-body-spacing/id-child-subtree-skipped ()
  "ID-bearing child subtrees are left to their own updates.
The parent's normalization stops at their heading and skips the whole
subtree, so their spacing cannot be double-handled."
  (should (equal (orgist-test--normalize
                  "* TODO Task
:PROPERTIES:
:ID:       T1
:END:
Description.
** TODO Child
:PROPERTIES:
:ID:       T2
:END:
Glued child body.

* TODO Next
:PROPERTIES:
:ID:       T3
:END:
")
                 "* TODO Task
:PROPERTIES:
:ID:       T1
:END:

Description.

** TODO Child
:PROPERTIES:
:ID:       T2
:END:
Glued child body.

* TODO Next
:PROPERTIES:
:ID:       T3
:END:
")))

(ert-deftest orgist-body-spacing/description-subheading-shape-is-stable ()
  "Normalizing again adds no blank line under a description sub-heading.
The pass that puts a blank line after a sub-heading's metadata used
to skip blank lines while looking for the end of that metadata, so
every pull added one more."
  (let ((shaped "* TODO Task
:PROPERTIES:
:ID:       T1
:END:

Description.

** Detail

Sub body.

** Logged
:LOGBOOK:
- Note taken on [2026-09-12 Sat 05:06]
:END:

Logged body.
* TODO Next
:PROPERTIES:
:ID:       T2
:END:
"))
    (should (equal (orgist-test--normalize shaped) shaped))
    (should (equal (orgist-test--normalize (orgist-test--normalize shaped)) shaped))))

(ert-deftest orgist-body-spacing/logbook-drawer-entries-stay-glued-to-end ()
  "No blank line goes inside a :LOGBOOK: drawer written in Org's default format.
Bare \"- State\" lines got a blank line before whatever followed them,
which in a drawer was its :END: line."
  (let ((org-log-note-headings (eval (car (get 'org-log-note-headings 'standard-value)) t)))
    (should (equal (orgist-test--normalize
                    "* TODO Task
:PROPERTIES:
:ID:       T1
:END:
:LOGBOOK:
- State \"TODO\"       from              [2026-09-12 Sat 05:06]
:END:
Description line.
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
- State \"TODO\"       from              [2026-09-12 Sat 05:06]
:END:

Description line.

* TODO Next
:PROPERTIES:
:ID:       T2
:END:
"))))

(provide 'test-body-spacing)
;;; test-body-spacing.el ends here
