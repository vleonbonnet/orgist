;;; test-element-identity.el --- Regression tests for element identity -*- lexical-binding: t; -*-

;; Usage: emacs --batch -L . -l ert -l test-element-identity.el -f ert-run-tests-batch-and-exit
;;
;; A TODO heading that carried an org-id before its first push (stamped
;; by org-store-link, capture or org-linker) used that org-id as the
;; item_add temp_id, and `orgist-remap-temp-ids' then overwrote :ID:
;; with the Todoist ID: every [[id:...]] link to the task dangled and
;; its ID-derived attachment directory moved.  Temporary IDs now go to
;; TODOIST_ID; the real ID takes :ID: only when the heading has none,
;; so an existing org-id is never touched and the Todoist ID lives in
;; TODOIST_ID beside it.

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

(defconst orgist-test--org-id "11111111-2222-3333-4444-555555555555"
  "An org-id UUID that links elsewhere point to.")

(defconst orgist-test--project-file
  ":PROPERTIES:
:ID:       PROJ1
:TODOIST-PROJECT:
:END:
#+TITLE: Proj
* TODO First
:PROPERTIES:
:ID:       T1
:TODOIST-ORDER: 0
:END:

* TODO Second
:PROPERTIES:
:ID:       T2
:TODOIST-ORDER: 1
:END:

")

(defmacro orgist-test--with-project (&rest body)
  "Run BODY with a throwaway orgist project directory as `orgist-base-dir'.
The project file holds two synced tasks whose snapshots match their
local state, so only the edits made by BODY can diff.  BODY runs in
the buffer visiting the project file."
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
          (orgist-sync-comments nil)
          (orgist-sync-attachments nil)
          (orgist-enable-write-back t)
          (orgist-project-buffer-cache nil))
     (with-temp-file file (insert orgist-test--project-file))
     (let ((buf (find-file-noselect file)))
       (unwind-protect
           (with-current-buffer buf
             (orgist-build-id-cache)
             (dolist (id '("T1" "T2"))
               (goto-char (orgist-find-element-by-id id))
               (puthash id (orgist-element-local-state) orgist-snapshots))
             ,@body)
         (with-current-buffer buf
           (set-buffer-modified-p nil))
         (kill-buffer buf)
         (delete-directory dir t)))))

(defun orgist-test--append (text)
  "Append TEXT to the project file and save it."
  (goto-char (point-max))
  (insert text)
  (save-buffer))

(defun orgist-test--goto-heading (title)
  "Move point to the heading whose text ends with TITLE."
  (goto-char (point-min))
  (re-search-forward (format "^\\*+ \\(?:[A-Z]+ \\)?%s$" (regexp-quote title)))
  (org-back-to-heading t))

(defun orgist-test--new-ids (changes kind)
  "Return the element IDs reported as KIND in CHANGES."
  (mapcar #'car (seq-filter (lambda (c) (eq (cdr c) kind)) changes)))

(defun orgist-test--remap (temp-id real-id)
  "Run `orgist-remap-temp-ids' as if item_add TEMP-ID returned REAL-ID."
  (orgist-remap-temp-ids
   (list (list (cons 'type "item_add")
               (cons 'temp_id temp-id)
               (cons 'args nil)))
   (list (cons (intern temp-id) real-id))))

(ert-deftest orgist-identity/accessors ()
  "TODOIST_ID wins over :ID:; binding a heading never overwrites an org-id."
  (with-temp-buffer
    (insert "* TODO Linked\n:PROPERTIES:\n:ID:       " orgist-test--org-id
            "\n:END:\n* TODO Plain\n")
    (org-mode)
    (orgist-test--goto-heading "Linked")
    (should (equal (orgist--element-id) orgist-test--org-id))
    (orgist--set-element-id "6Xreal")
    (should (equal (org-entry-get (point) "ID") orgist-test--org-id))
    (should (equal (org-entry-get (point) "TODOIST_ID") "6Xreal"))
    (should (equal (orgist--element-id) "6Xreal"))
    (orgist-test--goto-heading "Plain")
    (orgist--set-element-id "6Xplain")
    (should (equal (org-entry-get (point) "ID") "6Xplain"))
    (should-not (org-entry-get (point) "TODOIST_ID"))
    (should (orgist--temp-id-p orgist-test--org-id))
    (should-not (orgist--temp-id-p "6Xreal"))
    (set-buffer-modified-p nil)))

(ert-deftest orgist-identity/new-task-gets-its-todoist-id-in-id ()
  "A heading without any ID gets the real Todoist ID in :ID:, as before."
  (orgist-test--with-project
    (orgist-test--append "* TODO Fresh\n")
    (let* ((changes (orgist-diff-all-elements))
           (temp (car (orgist-test--new-ids changes 'new))))
      (should temp)
      (orgist-test--goto-heading "Fresh")
      ;; The temporary ID never touches :ID:.
      (should-not (org-entry-get (point) "ID"))
      (should (equal (org-entry-get (point) "TODOIST_ID") temp))
      (let ((add (seq-find (lambda (c) (equal (alist-get 'type c) "item_add"))
                           (orgist-changes-to-commands changes))))
        (should (equal (alist-get 'temp_id add) temp)))
      (orgist-test--remap temp "6Xfresh")
      (orgist-test--goto-heading "Fresh")
      (should (equal (org-entry-get (point) "ID") "6Xfresh"))
      (should-not (org-entry-get (point) "TODOIST_ID"))
      (should (= (orgist-find-element-by-id "6Xfresh") (point))))))

(ert-deftest orgist-identity/org-id-survives-the-push ()
  "A task with a pre-existing org-id keeps it; its Todoist ID goes to TODOIST_ID."
  (orgist-test--with-project
    (orgist-test--append
     (concat "* TODO Linked\n:PROPERTIES:\n:ID:       " orgist-test--org-id
             "\n:END:\n"))
    (let* ((changes (orgist-diff-all-elements))
           (temp (car (orgist-test--new-ids changes 'new))))
      (should temp)
      (should-not (equal temp orgist-test--org-id))
      (orgist-test--goto-heading "Linked")
      (should (equal (org-entry-get (point) "ID") orgist-test--org-id))
      (should (equal (org-entry-get (point) "TODOIST_ID") temp))
      (orgist-test--remap temp "6Xlinked")
      (orgist-test--goto-heading "Linked")
      (should (equal (org-entry-get (point) "ID") orgist-test--org-id))
      (should (equal (org-entry-get (point) "TODOIST_ID") "6Xlinked"))
      (should (= (orgist-find-element-by-id "6Xlinked") (point)))
      ;; A sub-task written under it is parented to the Todoist ID.
      (org-end-of-subtree t t)
      (insert "** TODO Child\n")
      (orgist-test--goto-heading "Child")
      (should (equal (plist-get (orgist-element-local-state) :parent-id)
                     "6Xlinked")))))

(ert-deftest orgist-identity/pending-temp-id-is-reemitted ()
  "A temporary ID whose creation never reached Todoist is re-sent, not re-minted."
  (orgist-test--with-project
    (orgist-test--append
     "* TODO Pending\n:PROPERTIES:\n:TODOIST_ID: aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee\n:END:\n")
    (let ((changes (orgist-diff-all-elements)))
      (should (equal (orgist-test--new-ids changes 'new)
                     '("aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee")))
      (orgist-test--goto-heading "Pending")
      (should (equal (org-entry-get (point) "TODOIST_ID")
                     "aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee")))))

(ert-deftest orgist-identity/real-ids-without-snapshot-are-not-readded ()
  "A dash-free Todoist ID whose snapshot went missing never becomes a duplicate."
  (orgist-test--with-project
    (orgist-test--append
     (concat "* TODO Orphan\n:PROPERTIES:\n:ID:       6Xorphan\n:END:\n"
             "* TODO Linked orphan\n:PROPERTIES:\n:ID:       " orgist-test--org-id
             "\n:TODOIST_ID: 6Xorphan2\n:END:\n"
             "* Real section\n:PROPERTIES:\n:ID:       6Xsection\n:SECTION:\n:END:\n"))
    (let ((changes (orgist-diff-all-elements)))
      (should-not (orgist-test--new-ids changes 'new))
      (should-not (orgist-test--new-ids changes 'new-section)))))

(ert-deftest orgist-identity/sections ()
  "A level-1 heading with an org-id becomes a section and keeps the org-id;
a section pending under an older orgist's :ID: temp ID is re-sent."
  (orgist-test--with-project
    (orgist-test--append
     (concat "* Notes\n:PROPERTIES:\n:ID:       " orgist-test--org-id "\n:END:\n"
             "* Legacy\n:PROPERTIES:\n:ID:       99999999-8888-7777-6666-555555555555\n"
             ":SECTION: t\n:END:\n"))
    (let* ((changes (orgist-diff-all-elements))
           (sections (orgist-test--new-ids changes 'new-section)))
      (should (= (length sections) 2))
      (should (member "99999999-8888-7777-6666-555555555555" sections))
      (orgist-test--goto-heading "Notes")
      (should (equal (org-entry-get (point) "ID") orgist-test--org-id))
      (should (org-entry-get (point) "SECTION"))
      (should (member (org-entry-get (point) "TODOIST_ID") sections)))))

(ert-deftest orgist-identity/clear-body-keeps-a-pending-child-task ()
  "A sub-task bound only through TODOIST_ID is a Todoist element, never body."
  (with-temp-buffer
    (insert "* TODO Parent\n:PROPERTIES:\n:ID:       6Xparent\n:END:\nOld description\n"
            "** TODO New child\n:PROPERTIES:\n:TODOIST_ID: aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee\n:END:\n")
    (org-mode)
    (orgist-test--goto-heading "Parent")
    (orgist-clear-body)
    (should-not (string-match-p "Old description" (buffer-string)))
    (should (string-match-p "^\\*\\* TODO New child$" (buffer-string)))
    (set-buffer-modified-p nil)))

(ert-deftest orgist-identity/browse-uses-the-todoist-id ()
  "Opening a task in Todoist uses TODOIST_ID, not the org-id beside it."
  (with-temp-buffer
    (insert "* TODO Linked\n:PROPERTIES:\n:ID:       " orgist-test--org-id
            "\n:TODOIST_ID: 6Xlinked\n:END:\n")
    (org-mode)
    (orgist-test--goto-heading "Linked")
    (let (opened
          (orgist-browse-url-format "todoist://task?id=%s"))
      (cl-letf (((symbol-function 'browse-url) (lambda (url &rest _) (setq opened url))))
        (orgist-browse-task))
      (should (equal opened "todoist://task?id=6Xlinked")))
    (set-buffer-modified-p nil)))

(provide 'test-element-identity)
;;; test-element-identity.el ends here
