;;; test-log-notes.el --- Regression tests for log notes in a task's body -*- lexical-binding: t; -*-

;; Usage: emacs --batch -L . -l ert -l test-log-notes.el -f ert-run-tests-batch-and-exit
;;
;; With `org-log-into-drawer' nil (Org's default), state changes and
;; notes are list items at the top of the body, not in a :LOGBOOK:
;; drawer.  A pull inserted the description above them, so a
;; description sub-heading took them as its own content: the log line
;; became description text and the next write-back pushed it to
;; Todoist.  The log notes are now located the same way everywhere
;; (`orgist--log-item-regexp', `orgist--goto-description-start'): the
;; description goes below them, extraction and clearing leave them out,
;; and notes that older pulls left after the description survive a
;; description change.

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

(defconst orgist-test--created
  (format "- State \"TODO\"       from              %s"
          (orgist-parse-todoist-timestamp "2026-09-12T12:06:00Z"))
  "The creation entry a pull writes for added_at 2026-09-12T12:06:00Z.")

(defmacro orgist-test--with-entry (text &rest body)
  "Run BODY in an Org buffer holding TEXT, point at the start.
Log notes go in the body, as with Org's default settings."
  (declare (indent 1))
  `(with-temp-buffer
     (let ((org-log-into-drawer nil)
           (org-log-note-headings (eval (car (get 'org-log-note-headings 'standard-value)) t))
           (org-log-states-order-reversed t)
           (orgist-enable-write-back nil)
           (orgist-sync-comments nil)
           (orgist-sync-attachments nil)
           (orgist-reminders nil))
       (insert ,text)
       (org-mode)
       (goto-char (point-min))
       (unwind-protect (progn ,@body)
         (set-buffer-modified-p nil)))))

(defun orgist-test--pull (description &rest more)
  "Apply a pull of task T1 with DESCRIPTION and MORE fields at point-min."
  (goto-char (point-min))
  (orgist-update-element
   (append more
           `((id . "T1") (content . "Task") (checked . :json-false) (priority . 1)
             (project_id . "P") (added_at . "2026-09-12T12:06:00Z")
             (description . ,description)))))

(defconst orgist-test--next
  "* TODO Next\n:PROPERTIES:\n:ID:       T2\n:END:\n")

(ert-deftest orgist-log-notes/description-goes-below-the-notes ()
  "A pulled description with a sub-heading leaves the log notes above it."
  (skip-unless (executable-find "pandoc"))
  (orgist-test--with-entry (concat "* TODO Task\n:PROPERTIES:\n:ID:       T1\n:END:\n"
                                   orgist-test--next)
    (orgist-test--pull "Intro.\n\n# Notes\n\nNote text.")
    (should (equal (buffer-string)
                   (concat "* TODO Task\n:PROPERTIES:\n:ID:       T1\n:END:\n"
                           orgist-test--created "\n\n"
                           "Intro.\n\n** Notes\n\nNote text.\n"
                           orgist-test--next)))
    (goto-char (point-min))
    (should (equal (orgist-extract-body-text) "Intro.\n\n* Notes\nNote text."))))

(ert-deftest orgist-log-notes/rebuilding-the-description-is-stable ()
  "Pulling the same description again rebuilds the same text."
  (skip-unless (executable-find "pandoc"))
  (orgist-test--with-entry (concat "* TODO Task\n:PROPERTIES:\n:ID:       T1\n:END:\n"
                                   orgist-test--next)
    (orgist-test--pull "Intro.\n\n# Notes\n\nNote text.")
    (let ((first (buffer-string)))
      (orgist-test--pull "Intro.\n\n# Notes\n\nNote text.")
      (should (equal (buffer-string) first)))))

(ert-deftest orgist-log-notes/notes-after-the-description-survive ()
  "Log notes an older pull left below the description outlive its change."
  (skip-unless (executable-find "pandoc"))
  (orgist-test--with-entry
      (concat "* DONE Task\nCLOSED: [2026-09-20 Sun 10:00]\n:PROPERTIES:\n:ID:       T1\n:END:\n"
              "Old text.\n"
              "- State \"DONE\"       from \"TODO\"       [2026-09-20 Sun 10:00]\n"
              orgist-test--created "\n"
              orgist-test--next)
    (should (equal (orgist-extract-body-text) "Old text."))
    (orgist-test--pull "New text." '(checked . t) '(completed_at . "2026-09-20T17:00:00Z"))
    (let ((text (buffer-string)))
      (should-not (string-match-p "Old text" text))
      (should (string-match-p
               (concat ":END:\n"
                       (regexp-quote "- State \"DONE\"       from \"TODO\"       [2026-09-20 Sun 10:00]")
                       "\n" (regexp-quote orgist-test--created) "\n\nNew text\\.\n")
               text)))
    (goto-char (point-min))
    (should (equal (orgist-extract-body-text) "New text."))))

(ert-deftest orgist-log-notes/multi-line-notes-are-not-description ()
  "A comment note with a blank continuation line stays out of the description."
  (orgist-test--with-entry
      (concat "* TODO Task\n:PROPERTIES:\n:ID:       T1\n:END:\n"
              "- [2026-09-12 Sat 05:06] Note \\\\\n  First line.\n  \n  Second paragraph.\n"
              "- Note taken on [2026-09-13 Sun 08:00] \\\\\n  Typed in org.\n"
              orgist-test--created "\n\n"
              "Description.\n\n- valve\n- clamp\n"
              orgist-test--next)
    (should (equal (orgist-extract-body-text) "Description.\n\n- valve\n- clamp"))
    (should (equal (orgist-clear-body) "Description.\n\n- valve\n- clamp\n"))
    (should (string-match-p "Second paragraph\\.\n- Note taken on" (buffer-string)))
    (should (string-match-p "Typed in org\\." (buffer-string)))))

(ert-deftest orgist-log-notes/custom-note-headings ()
  "Notes written with a customized `org-log-note-headings' are recognized."
  (orgist-test--with-entry
      (concat "* TODO Task\n:PROPERTIES:\n:ID:       T1\n:END:\n"
              "- [2026-09-12 Sat 05:06] From            to \"TODO\"\n"
              "Description.\n"
              orgist-test--next)
    (let ((org-log-note-headings '((done . "%t Closing Note")
                                   (state . "%t From %-10S to %-10s")
                                   (note . "%t Note"))))
      (should (equal (orgist-extract-body-text) "Description."))
      (orgist--normalize-body-spacing)
      (should (string-match-p "to \"TODO\"\n\nDescription\\.\n\n\\* TODO Next" (buffer-string))))))

(ert-deftest orgist-log-notes/bullet-description-is-not-a-note ()
  "A description that is a list is content, even right after the notes."
  (orgist-test--with-entry
      (concat "* TODO Task\n:PROPERTIES:\n:ID:       T1\n:END:\n"
              orgist-test--created "\n"
              "- State of the art\n- From the shed\n"
              orgist-test--next)
    (should (equal (orgist-extract-body-text) "- State of the art\n- From the shed"))
    (orgist--normalize-body-spacing)
    (should (string-match-p (concat (regexp-quote orgist-test--created) "\n\n- State of the art")
                            (buffer-string)))))

(ert-deftest orgist-log-notes/reminder-stamp-goes-below-the-notes ()
  "An absolute reminder's timestamp does not land above the log notes."
  (orgist-test--with-entry
      (concat "* TODO Task\n:PROPERTIES:\n:ID:       T1\n:END:\n"
              orgist-test--created "\n\n"
              "Description.\n"
              orgist-test--next)
    (orgist-apply-reminders '(((type . "absolute") (due . ((date . "2026-09-20"))))))
    (should (string-match-p (concat ":END:\n" (regexp-quote orgist-test--created) "\n")
                            (buffer-string)))
    (goto-char (point-min))
    (should (string-prefix-p "<2026-09-20" (orgist-extract-body-text)))))

(provide 'test-log-notes)
;;; test-log-notes.el ends here
