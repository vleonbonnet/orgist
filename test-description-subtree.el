;;; test-description-subtree.el --- Regression tests for task descriptions -*- lexical-binding: t; -*-

;; Usage: emacs --batch -L . -l ert -l test-description-subtree.el -f ert-run-tests-batch-and-exit
;;
;; A task's description used to stop at its first child heading.  The
;; Markdown headings of a Todoist description become child headings on
;; pull, and notes written in org under a task are child headings too,
;; so everything below the first one was invisible to write-back: edits
;; there never reached Todoist, an edit above it pushed a description
;; truncated at that heading (deleting the rest in Todoist), and the
;; next remote update rebuilt the body from Todoist, losing the local
;; edits.  A child Todoist task nested under such a heading was deleted
;; with it, and the next write-back pushed that deletion.
;;
;; The description is now the body plus every non-element heading
;; under the task, with levels relative to the task; a pull leaves the
;; body alone when Todoist's description did not change; replacing a
;; description moves nested tasks up instead of deleting them; and a
;; section's body, which has no Todoist counterpart, is never cleared.

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

(defconst orgist-test--task-body
  "Buy the anti-siphon model.

{1} Parts list                                                   :ATTACH:
:PROPERTIES:
:ID:       11111111-2222-3333-4444-555555555555
:END:
- valve
- clamp

{2} Sizes
1 inch

{1} TODO Call the plumber
:PROPERTIES:
:ID:       6Xchild
:TODOIST-ORDER: 0
:END:
Before Friday.

{1} Shed notes
Old valve is in the shed.
"
  "Body of a task, {N} marking a heading N levels below it: description
text, two description sub-headings (one with an org-id, tags and a
nested heading) and a child task.")

(defconst orgist-test--expected-description
  "Buy the anti-siphon model.

* Parts list
- valve
- clamp

** Sizes
1 inch

* Shed notes
Old valve is in the shed."
  "The description `orgist-test--task-body' extracts to.")

(defmacro orgist-test--with-task (level &rest body)
  "Run BODY on a task at LEVEL holding `orgist-test--task-body'."
  (declare (indent 1))
  `(with-temp-buffer
     (let ((stars (make-string ,level ?*))
           (orgist-snapshots (make-hash-table :test 'equal)))
       (insert stars " TODO Replace valve\n:PROPERTIES:\n:ID:       6Xtask\n:END:\n"
               (replace-regexp-in-string
                "^{1}" (make-string (1+ ,level) ?*)
                (replace-regexp-in-string "^{2}" (make-string (+ 2 ,level) ?*)
                                          orgist-test--task-body t t)
                t t))
       (org-mode)
       (goto-char (point-min))
       (unwind-protect (progn ,@body)
         (set-buffer-modified-p nil)))))

(ert-deftest orgist-description/extract-includes-sub-headings ()
  "Description sub-headings are part of the description; child tasks are not."
  (orgist-test--with-task 2
    (should (equal (orgist-extract-body-text) orgist-test--expected-description))))

(ert-deftest orgist-description/extract-is-depth-independent ()
  "A task's description does not change when the task moves to another depth."
  (dolist (level '(1 3))
    (orgist-test--with-task level
      (should (equal (orgist-extract-body-text) orgist-test--expected-description)))))

(ert-deftest orgist-description/extract-plain-body-unchanged ()
  "A task without sub-headings extracts exactly its body, as before."
  (with-temp-buffer
    (insert "* TODO Plain\n:PROPERTIES:\n:ID:       6Xplain\n:END:\n"
            "- State \"TODO\"       from              [2026-01-01 Thu 10:00]\n\n"
            "First paragraph.\n\nSecond paragraph.\n\n"
            "** TODO Child\n:PROPERTIES:\n:ID:       6Xc\n:END:\n")
    (org-mode)
    (goto-char (point-min))
    (should (equal (orgist-extract-body-text) "First paragraph.\n\nSecond paragraph."))
    (should (equal (orgist-extract-body-text) (orgist--entry-body-text)))
    (set-buffer-modified-p nil)))

(ert-deftest orgist-description/section-extracts-own-body-only ()
  "A section has no Todoist description: headings under it are never included."
  (with-temp-buffer
    (insert "* Garden\n:PROPERTIES:\n:ID:       6Xsec\n:SECTION:\n:END:\nSection text.\n"
            "** Notes\nNot a description.\n")
    (org-mode)
    (goto-char (point-min))
    (should (equal (orgist-extract-body-text) "Section text."))
    (set-buffer-modified-p nil)))

(ert-deftest orgist-description/spacing-keeps-sub-heading-drawers ()
  "Body spacing puts the blank line after a description sub-heading's drawer.
A blank line between a heading and its property drawer turns the
drawer into plain text, and the org-id in it stops resolving."
  (with-temp-buffer
    (let ((orgist-snapshots (make-hash-table :test 'equal)))
      (insert "* TODO Task\n:PROPERTIES:\n:ID:       6Xtask\n:END:\n\nIntro.\n"
              "** Notes\n:PROPERTIES:\n:ID:       11111111-2222-3333-4444-555555555555\n:END:\n"
              "Note text.\n")
      (org-mode)
      (goto-char (point-min))
      (orgist--normalize-body-spacing)
      (goto-char (point-min))
      (re-search-forward "^\\*\\* Notes$")
      (should (equal (org-entry-get (point) "ID") "11111111-2222-3333-4444-555555555555"))
      (should (string-match-p ":END:\n\nNote text\\." (buffer-string)))
      (set-buffer-modified-p nil))))

(ert-deftest orgist-description/clear-body-keeps-elements ()
  "Clearing removes the description and keeps child tasks."
  (orgist-test--with-task 2
    (orgist-clear-body)
    (let ((text (buffer-string)))
      (should-not (string-match-p "anti-siphon\\|Parts list\\|Sizes\\|Shed notes" text))
      (should (string-match-p "^\\*\\*\\* TODO Call the plumber$" text))
      (should (string-match-p "Before Friday\\." text)))))

(ert-deftest orgist-description/nested-task-is-moved-up-not-deleted ()
  "A task under a description heading becomes a direct child instead of being deleted."
  (with-temp-buffer
    (let ((orgist-snapshots (make-hash-table :test 'equal)))
      (insert "* TODO Parent\n:PROPERTIES:\n:ID:       6Xparent\n:END:\nIntro\n"
              "** Notes\nSome notes\n"
              "*** TODO Nested\n:PROPERTIES:\n:ID:       6Xnested\n:TODOIST-ORDER: 1\n:END:\nNested body\n"
              "** TODO Direct\n:PROPERTIES:\n:ID:       6Xdirect\n:TODOIST-ORDER: 0\n:END:\n")
      (org-mode)
      (orgist-build-id-cache)
      (goto-char (point-min))
      (orgist-clear-body)
      (let ((text (buffer-string)))
        (should-not (string-match-p "Notes\\|Some notes\\|Intro" text))
        ;; Both tasks remain direct children, ordered by TODOIST-ORDER.
        (should (string-match-p
                 "^\\*\\* TODO Direct\n\\(?:.*\n\\)*?\\*\\* TODO Nested\n" text))
        (should (string-match-p "Nested body" text)))
      (should (orgist-find-element-by-id "6Xnested"))
      (set-buffer-modified-p nil))))

(defconst orgist-test--project-file
  ":PROPERTIES:
:ID:       PROJ1
:TODOIST-PROJECT:
:END:
#+TITLE: Proj
* TODO Replace valve
:PROPERTIES:
:ID:       6Xtask
:TODOIST-ORDER: 0
:END:

Buy the anti-siphon model.

** Parts list
:PROPERTIES:
:ID:       11111111-2222-3333-4444-555555555555
:END:

- valve
- clamp

* Garden
:PROPERTIES:
:ID:       6Xsec
:SECTION:
:TODOIST-ORDER: 0
:END:

Zone map.

** Controller manual

Kept locally.
")

(defmacro orgist-test--with-project (&rest body)
  "Run BODY in a throwaway project whose task and section are snapshotted.
The task's snapshot records a Todoist description equal to its body,
as a pull does, so only edits made by BODY diff."
  (declare (indent 0))
  `(let* ((dir (make-temp-file "orgist-test-" t))
          (file (expand-file-name "Proj.org" dir))
          (orgist-base-dir dir)
          (orgist-snapshot-file (expand-file-name "snapshots.el" dir))
          (orgist-log-file nil)
          (orgist--write-back-stamps nil)
          (orgist--write-back-stamps-path nil)
          (orgist--pending-stamps nil)
          (orgist-snapshots (make-hash-table :test 'equal))
          (orgist-reminders nil)
          (orgist-labels nil)
          (orgist-sync-comments nil)
          (orgist-sync-attachments nil)
          (orgist-enable-write-back t)
          (orgist-project-buffer-cache nil))
     (with-temp-file file (insert orgist-test--project-file))
     (let ((buf (find-file-noselect file)))
       (unwind-protect
           (with-current-buffer buf
             (orgist-build-id-cache)
             (goto-char (orgist-find-element-by-id "6Xtask"))
             (puthash "6Xtask"
                      (plist-put (plist-put (orgist-element-local-state)
                                            :remote-description
                                            "Buy the anti-siphon model.\n\n# Parts list\n\n- valve\n- clamp")
                                 :reminder-stamps nil)
                      orgist-snapshots)
             (goto-char (orgist-find-element-by-id "6Xsec"))
             (puthash "6Xsec" (orgist-element-local-state) orgist-snapshots)
             ,@body)
         (with-current-buffer buf
           (set-buffer-modified-p nil))
         (kill-buffer buf)
         (delete-directory dir t)))))

(defun orgist-test--task-element (description)
  "Return a Todoist item alist for the fixture task with DESCRIPTION."
  `((id . "6Xtask") (content . "Replace valve") (description . ,description)
    (project_id . "PROJ1") (section_id . nil) (parent_id . nil)
    (checked . nil) (priority . 1) (labels . []) (child_order . 0)
    (added_at . "2026-01-01T10:00:00Z")))

(ert-deftest orgist-description/edit-below-sub-heading-is-pushed ()
  "An edit under a description sub-heading reaches Todoist with the whole description."
  (skip-unless (executable-find "pandoc"))
  (orgist-test--with-project
    (goto-char (point-min))
    (re-search-forward "^- clamp$")
    (insert "\n- gasket")
    (save-buffer)
    (let* ((changes (orgist-diff-all-elements))
           (diff (alist-get "6Xtask" changes nil nil #'equal))
           (update (seq-find (lambda (c) (equal (alist-get 'type c) "item_update"))
                             (orgist-changes-to-commands changes)))
           (md (alist-get 'description (alist-get 'args update))))
      (should (assq :description diff))
      (should md)
      (should (string-match-p "^Buy the anti-siphon model\\." md))
      (should (string-match-p "^# Parts list$" md))
      (should (string-match-p "gasket" md)))))

(ert-deftest orgist-description/unchanged-remote-keeps-the-body ()
  "A pull with an unchanged description leaves the body and its org-only parts alone.
A local edit made before the pull stays a pending change."
  (orgist-test--with-project
    (goto-char (point-min))
    (re-search-forward "^- clamp$")
    (insert "\n- gasket")
    (goto-char (orgist-find-element-by-id "6Xtask"))
    (orgist-update-element
     (orgist-test--task-element
      "Buy the anti-siphon model.\n\n# Parts list\n\n- valve\n- clamp"))
    ;; The description sub-heading keeps its org-id as a real property
    ;; (spacing must not detach the drawer from the heading), and the
    ;; local edit stays.
    (goto-char (point-min))
    (re-search-forward "^\\*\\* Parts list$")
    (should (equal (org-entry-get (point) "ID") "11111111-2222-3333-4444-555555555555"))
    (should (string-match-p "- gasket" (buffer-string)))
    ;; The snapshot still mirrors Todoist, so the edit is still pushed.
    (save-buffer)
    (let ((diff (alist-get "6Xtask" (orgist-diff-all-elements) nil nil #'equal)))
      (should (assq :description diff)))))

(ert-deftest orgist-description/changed-remote-replaces-the-body ()
  "A pull with a changed description rebuilds the body from Todoist."
  (skip-unless (executable-find "pandoc"))
  (orgist-test--with-project
    (goto-char (orgist-find-element-by-id "6Xtask"))
    (orgist-update-element
     (orgist-test--task-element "New plan.\n\n# Steps\n\n- shut off water"))
    (let ((text (buffer-string)))
      (should-not (string-match-p "anti-siphon\\|Parts list" text))
      (should (string-match-p "New plan\\." text))
      (should (string-match-p "^\\*\\* Steps$" text)))
    (goto-char (orgist-find-element-by-id "6Xtask"))
    (should (equal (plist-get (gethash "6Xtask" orgist-snapshots) :remote-description)
                   "New plan.\n\n# Steps\n\n- shut off water"))
    ;; Nothing local is pending after taking Todoist's version.
    (save-buffer)
    (should-not (alist-get "6Xtask" (orgist-diff-all-elements) nil nil #'equal))))

(ert-deftest orgist-description/section-body-is-never-cleared ()
  "Updating a section keeps the org-only content under it."
  (orgist-test--with-project
    (goto-char (orgist-find-element-by-id "6Xsec"))
    (orgist-update-element
     '((id . "6Xsec") (name . "Garden") (project_id . "PROJ1") (section_order . 0)))
    (let ((text (buffer-string)))
      (should (string-match-p "Zone map\\." text))
      (should (string-match-p "^\\*\\* Controller manual$" text))
      (should (string-match-p "Kept locally\\." text)))))

(ert-deftest orgist-description/push-records-the-sent-description ()
  "After pushing a description, the snapshot knows what Todoist now holds."
  (orgist-test--with-project
    (orgist-update-snapshots-from-local
     (list `((type . "item_update") (uuid . "u1")
             (args . ((id . "6Xtask") (description . "Sent text"))))))
    (should (equal (plist-get (gethash "6Xtask" orgist-snapshots) :remote-description)
                   "Sent text"))))

(ert-deftest orgist-description/confirm-reads-remote-like-local ()
  "The review adapter's live description compares equal to the pulled one."
  (skip-unless (executable-find "pandoc"))
  (let ((md "Intro text.\n\n# Parts\n\n- a\n- b"))
    (with-temp-buffer
      (insert "*** TODO Deep task\n" (orgist-convert-description md 3) "\n")
      (org-mode)
      (goto-char (point-min))
      (should (equal (orgist-extract-body-text)
                     (orgist--description-as-extracted md))))))

(ert-deftest orgist-description/deep-sub-headings-stay-headings ()
  "Sub-headings deeper than level 3 are pushed as Markdown headings, not paragraphs."
  (skip-unless (executable-find "pandoc"))
  (let ((md (orgist-convert-description-to-markdown
             "Intro.\n\n**** *Deep heading*\n- item\n\n****** Deeper\ntext")))
    (should (string-match-p "^#### \\*\\*Deep heading\\*\\*$" md))
    (should (string-match-p "^###### Deeper$" md))))

(defmacro orgist-test--with-legacy-snapshot (local-description &rest body)
  "Run BODY with task 6Xtask's snapshot in the pre-upgrade format.
CHANGES is bound to a description diff whose local side is
LOCAL-DESCRIPTION, and FETCHES counts calls to the stubbed fetch,
which returns the value of `remote'."
  (declare (indent 1))
  `(let* ((orgist-snapshots (make-hash-table :test 'equal))
          (orgist-snapshot-file (make-temp-file "orgist-snap-"))
          (orgist--snapshot-count-on-disk nil)
          (orgist-bearer-token "token")
          (orgist-log-file nil)
          (fetches 0)
          (changes (list (cons "6Xtask" (list (cons :description
                                                    (cons "Body only." ,local-description)))))))
     (puthash "6Xtask" (list :content "Task" :description "Body only.") orgist-snapshots)
     (unwind-protect
         (cl-letf (((symbol-function 'orgist--fetch-task-description)
                    (lambda (_id) (cl-incf fetches) remote)))
           ,@body)
       (delete-file orgist-snapshot-file))))

(ert-deftest orgist-description/legacy-diff-settled-when-local-matches-todoist ()
  "An unedited description is completed from Todoist instead of pushed."
  (skip-unless (executable-find "pandoc"))
  (let ((remote "Body only.\n\n# Notes\n\nFrom Todoist."))
    (orgist-test--with-legacy-snapshot "Body only.\n\n* Notes\nFrom Todoist."
      (should-not (orgist--settle-descriptions changes))
      (should (= fetches 1))
      (let ((snap (gethash "6Xtask" orgist-snapshots)))
        (should (equal (plist-get snap :remote-description) remote))
        (should (equal (plist-get snap :description)
                       "Body only.\n\n* Notes\nFrom Todoist."))))))

(ert-deftest orgist-description/legacy-diff-kept-when-locally-edited ()
  "A description edited locally keeps its diff and is pushed."
  (skip-unless (executable-find "pandoc"))
  (let ((remote "Body only.\n\n# Notes\n\nFrom Todoist."))
    (orgist-test--with-legacy-snapshot "Body only.\n\n* Notes\nEdited in org."
      (should (equal (orgist--settle-descriptions changes) changes))
      (should-not (plist-member (gethash "6Xtask" orgist-snapshots) :remote-description)))))

(ert-deftest orgist-description/legacy-diff-kept-when-fetch-fails ()
  "Without Todoist's description the diff stays: pushing is the safe side."
  (let ((remote nil))
    (orgist-test--with-legacy-snapshot "Body only.\n\n* Notes\nAnything."
      (should (equal (orgist--settle-descriptions changes) changes)))))

(ert-deftest orgist-description/current-snapshots-are-never-fetched ()
  "A snapshot that records Todoist's description is compared with it, not fetched."
  (let ((remote "irrelevant"))
    (orgist-test--with-legacy-snapshot "Body only.\n\n* Notes\nEdited."
      (puthash "6Xtask" (list :content "Task" :description "Body only."
                              :remote-description "Body only.")
               orgist-snapshots)
      (should (equal (orgist--settle-descriptions changes) changes))
      (should (= fetches 0)))))

(ert-deftest orgist-description/stale-snapshot-settles-against-recorded-todoist ()
  "A description already as in Todoist is not pushed, even with a stale snapshot.
The snapshot's description recorded seven copies of a section that an
edit outside orgist reduced to one, Todoist's text: the confirm buffer
offered to push a description identical to Todoist's, and showed an
empty diff."
  (skip-unless (executable-find "pandoc"))
  (let ((remote "irrelevant")
        (recorded "Body only.\n\n# Notes\n\nFrom Todoist."))
    (orgist-test--with-legacy-snapshot "Body only.\n\n* Notes\nFrom Todoist."
      (puthash "6Xtask" (list :content "Task"
                              :description "Body only.\n\n* Notes\nFrom Todoist.\n\n* Notes\nFrom Todoist."
                              :remote-description recorded)
               orgist-snapshots)
      (should-not (orgist--settle-descriptions changes))
      (should (= fetches 0))
      (let ((snap (gethash "6Xtask" orgist-snapshots)))
        (should (equal (plist-get snap :description) "Body only.\n\n* Notes\nFrom Todoist."))
        (should (equal (plist-get snap :remote-description) recorded))))))

(ert-deftest orgist-description/extraction-carries-no-text-properties ()
  "An extracted description is plain text, whatever the buffer displays.
With `org-indent-mode', headings carry `line-prefix' properties, which
leaked into snapshots and the snapshot file."
  (orgist-test--with-task 1
    (org-indent-mode 1)
    (font-lock-ensure)
    (let ((description (orgist-extract-body-text)))
      (should (equal description orgist-test--expected-description))
      (should-not (next-property-change 0 description))
      (should-not (text-properties-at 0 description)))))

(ert-deftest orgist-description/loaded-snapshots-carry-no-text-properties ()
  "Strings read back from the snapshot file lose any text properties."
  (let* ((orgist-snapshot-file (make-temp-file "orgist-snap-"))
         (orgist-snapshots nil)
         (orgist-log-file nil))
    (unwind-protect
        (progn
          (with-temp-file orgist-snapshot-file
            (insert ";; orgist snapshots -- do not edit\n"
                    "((\"T1\" :content \"Task\" :description #(\"** Notes\" 0 2 (line-prefix \"*\"))))\n"))
          (orgist-load-snapshots t)
          (let ((description (plist-get (gethash "T1" orgist-snapshots) :description)))
            (should (equal description "** Notes"))
            (should-not (text-properties-at 0 description))))
      (delete-file orgist-snapshot-file))))

(provide 'test-description-subtree)
;;; test-description-subtree.el ends here
