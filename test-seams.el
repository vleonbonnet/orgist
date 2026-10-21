;;; test-seams.el --- Tests for orgist's seams (M1) -*- lexical-binding: t; -*-

;; Usage: emacs --batch -L . -l ert -l test-seams.el -f ert-run-tests-batch-and-exit
;;
;; Orgist found its files by listing `orgist-base-dir' in 26 places;
;; overlay mode needs files that live elsewhere, so one registry now
;; answers which files orgist manages, and lookups of an element's
;; buffer go through it.

(add-to-list 'load-path
             (expand-file-name "~/.emacs.d/elpaca/builds/request"))
(dolist (dir '("~/.emacs.d/elpaca/builds/org-sync-confirm"
               "~/.emacs.d/elpaca/sources/org-sync-confirm"))
  (when (file-directory-p (expand-file-name dir))
    (add-to-list 'load-path (expand-file-name dir))))
(add-to-list 'load-path default-directory)
(require 'test-isolation)
(require 'ert)
(require 'orgist)

(defmacro orgist-test--with-files (files &rest body)
  "Run BODY with `orgist-base-dir' holding FILES, an alist (NAME . TEXT).
Buffers visiting them are killed afterwards."
  (declare (indent 1))
  `(let ((orgist-base-dir (file-name-as-directory (make-temp-file "orgist-seams-" t))))
     (dolist (file ,files)
       (with-temp-file (expand-file-name (car file) orgist-base-dir)
         (insert (cdr file))))
     (unwind-protect (progn ,@body)
       (dolist (buf (buffer-list))
         (when (and (buffer-file-name buf)
                    (string-prefix-p (expand-file-name orgist-base-dir)
                                     (expand-file-name (buffer-file-name buf))))
           (with-current-buffer buf (set-buffer-modified-p nil))
           (kill-buffer buf)))
       (delete-directory orgist-base-dir t))))

(defun orgist-test--project (id task-id)
  "Text of a project file with ID holding task TASK-ID."
  (format ":PROPERTIES:\n:ID:       %s\n:TODOIST-PROJECT:\n:END:\n#+TITLE: P\n* TODO Task\n:PROPERTIES:\n:ID:       %s\n:END:\n"
          id task-id))

(defun orgist-test--visited-p (name)
  (find-buffer-visiting (expand-file-name name orgist-base-dir)))

(ert-deftest orgist-seams/project-files-are-org-files-only ()
  "Dotfiles, backups and other files are not project files."
  (orgist-test--with-files '(("A.org" . "") ("B.org" . "") (".#A.org" . "")
                             ("A.org~" . "") ("notes.txt" . "") ("snapshots.el" . ""))
    (should (equal (mapcar #'file-name-nondirectory (orgist--project-files))
                   '("A.org" "B.org")))))

(ert-deftest orgist-seams/lookup-without-visit-ignores-unvisited-files ()
  "Without VISIT only files already visited are searched."
  (orgist-test--with-files `(("A.org" . ,(orgist-test--project "PA" "TA"))
                             ("B.org" . ,(orgist-test--project "PB" "TB")))
    (should-not (orgist--element-location "TB"))
    (should-not (orgist-test--visited-p "B.org"))
    (find-file-noselect (expand-file-name "B.org" orgist-base-dir))
    (should (eq (car (orgist--element-location "TB")) (orgist-test--visited-p "B.org")))))

(ert-deftest orgist-seams/lookup-visits-files-only-until-found ()
  "With VISIT files are visited in order and the search stops at the match."
  (orgist-test--with-files `(("A.org" . ,(orgist-test--project "PA" "TA"))
                             ("B.org" . ,(orgist-test--project "PB" "TB"))
                             ("C.org" . ,(orgist-test--project "PC" "TC")))
    (let ((location (orgist--element-location "TB" t)))
      (should (eq (car location) (orgist-test--visited-p "B.org")))
      (should (orgist-test--visited-p "A.org"))
      (should-not (orgist-test--visited-p "C.org")))))

(ert-deftest orgist-seams/at-element-runs-at-the-heading ()
  "BODY runs at the element's heading, its value is returned, point is kept."
  (orgist-test--with-files `(("A.org" . ,(orgist-test--project "PA" "TA")))
    (let ((buf (find-file-noselect (expand-file-name "A.org" orgist-base-dir))))
      (with-current-buffer buf (goto-char (point-max)))
      (should (equal (orgist--at-element "TA" nil (org-get-heading t t t t)) "Task"))
      (should (= (orgist--at-element "PA" nil (point)) 1))
      (with-current-buffer buf (should (= (point) (point-max)))))))

(ert-deftest orgist-seams/at-element-skips-body-when-absent ()
  "BODY is not evaluated when no project buffer holds the element."
  (orgist-test--with-files `(("A.org" . ,(orgist-test--project "PA" "TA")))
    (let ((ran nil))
      (should-not (orgist--at-element "missing" t (setq ran t)))
      (should-not ran))))

;;; Remote backend
;;
;; A validation run against real data on 2026-10-03 guarded HTTP
;; requests by method and still sent a real item_update: orgist's
;; command request carries no :type.  Writes are now refused by
;; operation, before any request is built.

(defmacro orgist-test--recording-requests (&rest body)
  "Run BODY with `request' and the retry wrapper recording, not sending.
Binds CALLS to the list of (URL . ARGS), oldest first."
  (declare (indent 0))
  `(let ((calls nil))
     (cl-letf (((symbol-function 'request)
                (lambda (url &rest args) (setq calls (append calls (list (cons url args)))) nil))
               ((symbol-function 'orgist--request-with-retry)
                (lambda (url &rest args) (setq calls (append calls (list (cons url args)))) nil))
               ((symbol-function 'url-copy-file)
                (lambda (url file &rest _) (setq calls (append calls (list (list url :file file)))) nil)))
       ,@body)))

(ert-deftest orgist-seams/every-operation-has-a-todoist-endpoint ()
  "The operation table and the Todoist endpoints name the same operations."
  (should (equal (sort (mapcar #'car orgist-todoist--endpoints) #'string<)
                 (sort (remq 'download (mapcar #'car orgist--remote-operations))
                       #'string<))))

(ert-deftest orgist-seams/read-only-refuses-every-write ()
  "While read-only, no write operation reaches `request'."
  (let ((orgist-read-only t)
        (orgist-log-file nil))
    (orgist-test--recording-requests
      (dolist (op orgist--remote-operations)
        (when (eq (cdr op) 'write)
          (should-error (orgist-remote (car op) :path '("1") :data '(("commands" . "[]")))
                        :type 'orgist-read-only)))
      (should-not calls))))

(ert-deftest orgist-seams/reads-pass-in-read-only-mode ()
  "Reads go through with their endpoint, method and authorization."
  (let ((orgist-read-only t)
        (orgist-bearer-token (lambda () "secret")))
    (orgist-test--recording-requests
      (orgist-remote 'get-task :path '("42") :retry t)
      (orgist-remote 'comments :params '(("project_id" . "7")))
      (orgist-remote 'download :url "https://files.example/x" :file "/tmp/x")
      (should (= (length calls) 3))
      (pcase-let ((`(,url . ,args) (nth 0 calls)))
        (should (equal url "https://api.todoist.com/api/v1/tasks/42"))
        (should (equal (plist-get args :type) "GET"))
        (should (equal (cdr (assoc "Authorization" (plist-get args :headers)))
                       "Bearer secret"))
        (should-not (plist-member args :path))
        (should-not (plist-member args :retry)))
      (should (equal (car (nth 1 calls)) "https://api.todoist.com/api/v1/comments"))
      (should (equal (plist-get (cdr (nth 1 calls)) :params) '(("project_id" . "7"))))
      (should (equal (nth 2 calls) '("https://files.example/x" :file "/tmp/x"))))))

(ert-deftest orgist-seams/commands-are-posted-with-extra-headers ()
  "The command request is a POST and keeps its own headers."
  (let ((orgist-bearer-token "t"))
    (orgist-test--recording-requests
      (orgist-remote 'commands :retry t :headers '(("Connection" . "close"))
                     :data '(("commands" . "[]")))
      (pcase-let ((`(,url . ,args) (car calls)))
        (should (equal url "https://api.todoist.com/api/v1/sync"))
        (should (equal (plist-get args :type) "POST"))
        (should (equal (plist-get args :headers)
                       '(("Authorization" . "Bearer t") ("Connection" . "close"))))))))

(ert-deftest orgist-seams/read-only-write-back-stops-before-detection ()
  "Read-only write-back neither scans for changes nor sends anything.
Detection binds new headings to temporary IDs, which already edits files."
  (let ((orgist-read-only t)
        (orgist-enable-write-back t)
        (orgist-log-file nil))
    (cl-letf (((symbol-function 'orgist-diff-all-elements)
               (lambda () (error "Scanned for changes")))
              ((symbol-function 'orgist-execute-write-back)
               (lambda (&rest _) (error "Sent commands"))))
      (should-not (orgist-write-back)))))

;;; File policy
;;
;; A mirror file is Todoist's, so a remote change may do anything to
;; it.  The policy names what overlay files will refuse; where a policy
;; says no, the file is left alone.

(ert-deftest orgist-seams/mirror-policy-by-default ()
  "Files are mirror files unless an override matches; first match wins."
  (should (equal (orgist-file-policy "/x/A.org") orgist--mirror-policy))
  (let ((orgist--file-policies '(("/Notes/" :mode overlay :restructure nil)
                                 ("\\.org\\'" :description none))))
    (should (eq (orgist--policy :mode "/x/Notes/a.org") 'overlay))
    (should-not (orgist--policy :restructure "/x/Notes/a.org"))
    ;; Unset properties keep the mirror answer.
    (should (eq (orgist--policy :description "/x/Notes/a.org") 'sync))
    (should (eq (orgist--policy :description "/x/B.org") 'none))
    (should (orgist--policy :restructure "/x/B.org"))))

(ert-deftest orgist-seams/policy-keeps-a-deleted-project-file ()
  "A remote project deletion leaves a file whose policy keeps its structure."
  (orgist-test--with-files `(("A.org" . ,(orgist-test--project "PA" "TA"))
                             ("B.org" . ,(orgist-test--project "PB" "TB")))
    (let ((orgist-log-file nil)
          (orgist-history-directory nil)
          (orgist-project-buffer-cache nil)
          (orgist--file-policies '(("/A\\.org\\'" :restructure nil))))
      (orgist-delete-project "PA")
      (should (file-exists-p (expand-file-name "A.org" orgist-base-dir)))
      (orgist-delete-project "PB")
      (should-not (file-exists-p (expand-file-name "B.org" orgist-base-dir))))))

(ert-deftest orgist-seams/policy-keeps-a-renamed-project-file ()
  "A project renamed in Todoist keeps its file name when the policy says so."
  (orgist-test--with-files `(("A.org" . ,(orgist-test--project "PA" "TA")))
    (let ((orgist-log-file nil)
          (orgist--file-policies '(("/A\\.org\\'" :restructure nil)))
          (buf (find-file-noselect (expand-file-name "A.org" orgist-base-dir))))
      (orgist-update-root-project-in-place '((id . "PA") (name . "Renamed")) buf)
      (should (file-exists-p (expand-file-name "A.org" orgist-base-dir)))
      (should-not (file-exists-p (expand-file-name "Renamed.org" orgist-base-dir)))
      ;; The title is a field, not structure: it still follows Todoist.
      (should (string-match-p "^#\\+TITLE: Renamed$"
                              (with-current-buffer buf (buffer-string)))))))

(ert-deftest orgist-seams/description-policy-none-keeps-the-body ()
  "With description policy `none', a pull leaves the body and write-back ignores it."
  (orgist-test--with-files `(("A.org" . ,(concat (orgist-test--project "PA" "TA")
                                                 "Local notes.\n")))
    (let ((orgist--file-policies '(("/A\\.org\\'" :description none)))
          (orgist-log-file nil)
          (orgist-enable-write-back t)
          (orgist-snapshots (make-hash-table :test 'equal))
          (orgist-snapshot-file (expand-file-name "snapshots.el" orgist-base-dir))
          (orgist-sync-comments nil)
          (orgist-sync-attachments nil)
          (orgist-reminders nil))
      (with-current-buffer (find-file-noselect (expand-file-name "A.org" orgist-base-dir))
        (goto-char (orgist-find-element-by-id "TA"))
        (orgist-update-element '((id . "TA") (content . "Task") (description . "Remote text.")
                                 (project_id . "PA") (checked . :json-false) (priority . 1)))
        (should (string-match-p "Local notes\\." (buffer-string)))
        (should-not (string-match-p "Remote text" (buffer-string)))
        (goto-char (point-max))
        (insert "More notes.\n")
        (should-not (assq :description (orgist-diff-element "TA")))))))

(defvar orgist-test--new-heading-id nil
  "Element ID the last scan bound the new heading to.")

(defun orgist-test--scan-new-headings ()
  "Add an unbound TODO heading to A.org and return the scan's changes."
  (let ((orgist-log-file nil)
        (orgist-snapshot-file (expand-file-name "snapshots.el" orgist-base-dir))
        (orgist--write-back-stamps nil)
        (orgist--write-back-stamps-path nil)
        (orgist--pending-stamps nil)
        (orgist-snapshots (make-hash-table :test 'equal))
        (orgist-sync-comments nil)
        (orgist-sync-attachments nil)
        (orgist-enable-write-back t))
    (with-current-buffer (find-file-noselect (expand-file-name "A.org" orgist-base-dir))
      (orgist-build-id-cache)
      (goto-char (orgist-find-element-by-id "TA"))
      (puthash "TA" (orgist-element-local-state) orgist-snapshots)
      (goto-char (point-max))
      (insert "* TODO Local only\n")
      (save-buffer)
      (prog1 (orgist-diff-all-elements)
        (goto-char (point-max))
        (org-back-to-heading t)
        (should (equal (org-get-heading t t t t) "Local only"))
        (setq orgist-test--new-heading-id (orgist--element-id))))))

(ert-deftest orgist-seams/policy-without-exposure-adds-no-element ()
  "An unbound TODO heading becomes a new task only where the policy exposes it."
  (orgist-test--with-files `(("A.org" . ,(orgist-test--project "PA" "TA")))
    (let ((orgist--file-policies '(("/A\\.org\\'" :expose nil))))
      (should-not (rassq 'new (orgist-test--scan-new-headings)))
      (should-not orgist-test--new-heading-id)))
  (orgist-test--with-files `(("A.org" . ,(orgist-test--project "PA" "TA")))
    (should (rassq 'new (orgist-test--scan-new-headings)))
    (should orgist-test--new-heading-id)))

;;; Subprocess snapshot hand-back
;;
;; A subprocess used to save its whole snapshot table, which the parent
;; then reloaded: a write-back during the run lost its snapshot updates
;; and its changes diffed again.  The subprocess now hands back only the
;; fields it changed, and the parent merges them.

(defun orgist-test--table (&rest entries)
  "A snapshot table from ENTRIES, each (ID . PLIST)."
  (let ((table (make-hash-table :test 'equal)))
    (dolist (entry entries)
      (puthash (car entry) (copy-tree (cdr entry)) table))
    table))

(ert-deftest orgist-seams/snapshot-delta-names-only-changes ()
  "The delta holds changed fields, new entries and removals."
  (let* ((base (orgist-test--table '("A" :content "a" :due nil)
                                   '("B" :content "b")
                                   '("C" :content "c" :order 1)))
         (current (orgist-test--table '("A" :content "a" :due "<2026-10-04>")
                                      '("C" :content "c")
                                      '("D" :content "d"))))
    (should (equal (sort (orgist--snapshot-delta base current)
                         (lambda (x y) (string< (car x) (car y))))
                   '(("A" :due "<2026-10-04>")
                     ("B" . removed)
                     ("C" :order nil)
                     ("D" :content "d"))))))

(ert-deftest orgist-seams/snapshot-merge-keeps-changes-made-meanwhile ()
  "A field or entry the parent changed during the run keeps the parent's value."
  (let* ((base (orgist-test--table '("A" :content "old" :due "d1")
                                   '("B" :content "b")
                                   '("C" :content "c")
                                   '("E" :content "e")))
         ;; Meanwhile, a write-back: A's title pushed, C changed, E deleted.
         (orgist-snapshots (orgist-test--table '("A" :content "pushed" :due "d1")
                                               '("B" :content "b")
                                               '("C" :content "c2")))
         (delta '(("A" :content "pulled" :due "d2")
                  ("B" . removed)
                  ("C" . removed)
                  ("E" :due "d3")
                  ("N" :content "new"))))
    (orgist--merge-snapshot-delta delta base)
    ;; A: the pushed title stays, the pulled due date lands.
    (should (equal (gethash "A" orgist-snapshots) '(:content "pushed" :due "d2")))
    (should-not (gethash "B" orgist-snapshots))
    (should (equal (gethash "C" orgist-snapshots) '(:content "c2")))
    (should-not (gethash "E" orgist-snapshots))
    (should (equal (gethash "N" orgist-snapshots) '(:content "new")))))

(ert-deftest orgist-seams/large-snapshot-files-read-back ()
  "Thousands of entries read back, as a table and as a delta.
Decoding the whole list at once recursed down its tail and exceeded
the evaluation depth: on real data (4555 snapshots) the rebuild read
back nothing and reported that its shadow pull had failed."
  (let* ((dir (make-temp-file "orgist-large-" t))
         (file (expand-file-name "table.el" dir))
         (table (make-hash-table :test 'equal)))
    (unwind-protect
        (progn
          (dotimes (i 5000)
            (puthash (format "T%d" i) (list :content (format "Tâche %d" i) :order i) table))
          (orgist--write-snapshot-table table file)
          (let ((back (orgist--read-snapshot-table file)))
            (should (= (hash-table-count back) 5000))
            (should (equal (gethash "T4999" back) '(:content "Tâche 4999" :order 4999))))
          (let ((orgist--snapshot-delta-file file)
                (orgist--snapshot-base (make-hash-table :test 'equal))
                (orgist-snapshots table))
            (orgist--write-snapshot-delta)
            (should (= (length (orgist--read-snapshot-delta file)) 5000))))
      (delete-directory dir t))))

(ert-deftest orgist-seams/snapshot-delta-survives-the-file ()
  "A delta written by a subprocess reads back equal, with multibyte text."
  (let* ((dir (make-temp-file "orgist-delta-" t))
         (orgist--snapshot-delta-file (expand-file-name "delta.el" dir))
         (orgist--snapshot-base (orgist-test--table '("A" :content "a")))
         (orgist-snapshots (orgist-test--table '("A" :content "Café – 2 €")))
         (orgist-snapshot-file (expand-file-name "snapshots.el" dir))
         (orgist--snapshot-count-on-disk nil)
         (orgist-log-file nil))
    (unwind-protect
        (progn
          (orgist-save-snapshots)
          (let ((delta (orgist--read-snapshot-delta orgist--snapshot-delta-file)))
            (should (equal delta '(("A" :content "Café – 2 €"))))
            (should (multibyte-string-p (plist-get (cdar delta) :content)))))
      (delete-directory dir t))))

;;; Sole writer
;;
;; A background sync used to save project files behind Emacs: a buffer
;; edited while it ran later overwrote its result, and a file saved
;; meanwhile was overwritten by it.  With `orgist-sole-writer' the
;; subprocess works on a staged copy, and Emacs takes over only what
;; nobody touched; a file edited meanwhile is deferred to the next sync.

(defmacro orgist-test--with-staging (files &rest body)
  "Run BODY with FILES in `orgist-base-dir', staged as `orgist-sole-writer' does.
Binds STAGING to the staging, and DIR to its directory."
  (declare (indent 1))
  `(orgist-test--with-files ,files
     (let* ((orgist-log-file nil)
            (orgist-history-directory nil)
            (orgist-sync-token-filename (expand-file-name "sync_token" orgist-base-dir))
            (orgist-snapshot-file (expand-file-name "snapshots.el" orgist-base-dir))
            (orgist-labels-file (expand-file-name "labels.el" orgist-base-dir))
            (staging (orgist--stage-project-files "test"))
            (dir (car staging)))
       ,@body)))

(defun orgist-test--append-to (file text)
  "Append TEXT to FILE on disk."
  (write-region text nil file t 'silent))

(defun orgist-test--file-string (file)
  (with-temp-buffer (insert-file-contents file) (buffer-string)))

(ert-deftest orgist-seams/sole-writer-takes-over-untouched-files-only ()
  "A file nobody touched takes the sync's result; edited or saved ones are deferred."
  (orgist-test--with-staging `(("A.org" . ,(orgist-test--project "PA" "TA"))
                               ("B.org" . ,(orgist-test--project "PB" "TB"))
                               ("C.org" . ,(orgist-test--project "PC" "TC"))
                               ("sync_token" . "old-token"))
    (let ((a (find-file-noselect (expand-file-name "A.org" orgist-base-dir)))
          (b (find-file-noselect (expand-file-name "B.org" orgist-base-dir))))
      ;; The subprocess changes all three and advances the token.
      (dolist (name '("A.org" "B.org" "C.org"))
        (orgist-test--append-to (expand-file-name name dir) "* TODO Pulled\n"))
      (with-temp-file (expand-file-name "sync_token" dir) (insert "new-token"))
      ;; Meanwhile B is edited, C is saved by someone else.
      (with-current-buffer b (goto-char (point-max)) (insert "Typed\n"))
      (orgist-test--append-to (expand-file-name "C.org" orgist-base-dir) "Saved meanwhile\n")
      (let ((deferred-ids (orgist--finish-staged-run "test" staging)))
        ;; A: taken over, buffer refreshed and saved.
        (with-current-buffer a
          (should (string-match-p "Pulled" (buffer-string)))
          (should-not (buffer-modified-p)))
        (should (string-match-p "Pulled" (orgist-test--file-string
                                          (expand-file-name "A.org" orgist-base-dir))))
        ;; B: the edit stays, the sync's change waits.
        (with-current-buffer b
          (should (string-match-p "Typed" (buffer-string)))
          (should-not (string-match-p "Pulled" (buffer-string))))
        (should-not (string-match-p "Pulled" (orgist-test--file-string
                                              (expand-file-name "B.org" orgist-base-dir))))
        ;; C: the save stays.
        (let ((c (orgist-test--file-string (expand-file-name "C.org" orgist-base-dir))))
          (should (string-match-p "Saved meanwhile" c))
          (should-not (string-match-p "Pulled" c)))
        ;; The deferred files' elements keep their snapshots, and the
        ;; token stays so the next sync fetches their changes again.
        (dolist (id '("PB" "TB" "PC" "TC"))
          (should (gethash id deferred-ids)))
        (should-not (gethash "TA" deferred-ids))
        (should (equal (orgist-test--file-string orgist-sync-token-filename) "old-token"))
        (should-not (file-directory-p dir))))))

(ert-deftest orgist-seams/sole-writer-commits-state-without-deferral ()
  "With every file taken over, the advanced sync token is taken over too."
  (orgist-test--with-staging `(("A.org" . ,(orgist-test--project "PA" "TA"))
                               ("sync_token" . "old-token"))
    (orgist-test--append-to (expand-file-name "A.org" dir) "* TODO Pulled\n")
    (with-temp-file (expand-file-name "sync_token" dir) (insert "new-token"))
    (should (= (hash-table-count (orgist--finish-staged-run "test" staging)) 0))
    (should (equal (orgist-test--file-string orgist-sync-token-filename) "new-token"))))

(ert-deftest orgist-seams/sole-writer-creates-removes-and-renames ()
  "New, removed and renamed project files are mirrored; an open buffer follows a rename."
  (orgist-test--with-staging `(("A.org" . ,(orgist-test--project "PA" "TA"))
                               ("B.org" . ,(orgist-test--project "PB" "TB")))
    (let ((a (find-file-noselect (expand-file-name "A.org" orgist-base-dir)))
          (b (find-file-noselect (expand-file-name "B.org" orgist-base-dir))))
      (ignore a)
      ;; The subprocess trashes A, renames B to Renamed, creates D.
      (delete-file (expand-file-name "A.org" dir))
      (rename-file (expand-file-name "B.org" dir) (expand-file-name "Renamed.org" dir))
      (orgist-test--append-to (expand-file-name "Renamed.org" dir) "* TODO After rename\n")
      (with-temp-file (expand-file-name "D.org" dir) (insert (orgist-test--project "PD" "TD")))
      (orgist--finish-staged-run "test" staging)
      (should-not (file-exists-p (expand-file-name "A.org" orgist-base-dir)))
      (should (directory-files-recursively (orgist--trash-directory) "\\`A\\.org\\'"))
      (should-not (orgist-test--visited-p "A.org"))
      (should-not (file-exists-p (expand-file-name "B.org" orgist-base-dir)))
      (should (file-exists-p (expand-file-name "Renamed.org" orgist-base-dir)))
      (with-current-buffer b
        (should (equal (file-name-nondirectory (buffer-file-name)) "Renamed.org"))
        (should (string-match-p "After rename" (buffer-string)))
        (should-not (buffer-modified-p)))
      (should (file-exists-p (expand-file-name "D.org" orgist-base-dir))))))

(ert-deftest orgist-seams/element-ids-do-not-depend-on-the-current-buffer ()
  "IDs read from file text stop at the line end whatever buffer is current.
A process sentinel runs in whatever buffer is current; in an
Emacs-Lisp buffer a newline is not whitespace, and the IDs of a
deferred file ran on into the next line and matched no snapshot."
  (let ((text (orgist-test--project "PA" "TA")))
    (with-temp-buffer
      (emacs-lisp-mode)
      (should (equal (sort (orgist--text-element-ids text) #'string<) '("PA" "TA")))
      (should (equal (orgist--text-project-id text) "PA")))))

(ert-deftest orgist-seams/sole-writer-keeps-line-ends ()
  "Staging and taking over keep a file's CRLF line ends."
  (orgist-test--with-files '(("A.org" . ""))
    (let ((file (expand-file-name "A.org" orgist-base-dir)))
      (let ((coding-system-for-write 'utf-8-dos))
        (write-region (orgist-test--project "PA" "TA") nil file nil 'silent))
      (let* ((orgist-log-file nil)
             (orgist-history-directory nil)
             (staging (orgist--stage-project-files "test"))
             (staged (expand-file-name "A.org" (car staging))))
        (with-current-buffer (find-file-noselect staged)
          (goto-char (point-max))
          (insert "* TODO Pulled\n")
          (save-buffer)
          (kill-buffer))
        (orgist--finish-staged-run "test" staging)
        (with-temp-buffer
          (set-buffer-multibyte nil)
          (insert-file-contents-literally file)
          (should (string-match-p "Pulled\r\n" (buffer-string)))
          (should-not (string-match-p "[^\r]\n" (buffer-string))))))))

;;; Subprocess token
;;
;; The token used to be part of the program on the subprocess's command
;; line, which other processes can read, and a failing subprocess's
;; backtrace printed that command line into orgist's log.

(defconst orgist-test--token "tok-SECRET-123"
  "A bearer token that must never show up in a command line or the log.")

(defmacro orgist-test--relaying (&rest body)
  "Run BODY with `orgist-log' and `message' recording.
Binds LOGGED and MESSAGES to the texts they received, oldest first."
  (declare (indent 0))
  `(let ((logged nil)
         (messages nil))
     (cl-letf (((symbol-function 'orgist-log)
                (lambda (_level format-string &rest args)
                  (setq logged (append logged (list (apply #'format format-string args))))))
               ((symbol-function 'message)
                (lambda (format-string &rest args)
                  (setq messages (append messages (list (apply #'format format-string args)))))))
       ,@body)))

(ert-deftest orgist-seams/relay-redacts-a-token-split-across-chunks ()
  "Lines are only looked at once complete, so a split token is redacted too."
  (orgist-test--relaying
    (let ((relay (orgist--subprocess-output-relay "orgist-test" orgist-test--token)))
      (funcall relay "one\nAuthorization: Bearer tok-SE")
      (funcall relay "CRET-123 sent\n\nOrgist [WARN] Bad tok-SECRET-123\ntail tok-SECRET")
      (funcall relay "-123")
      (should (equal logged '("orgist-test: one"
                              "orgist-test: Authorization: Bearer <orgist-bearer-token> sent"
                              "orgist-test: Orgist [WARN] Bad <orgist-bearer-token>")))
      (funcall relay nil)
      (should (equal (car (last logged)) "orgist-test: tail <orgist-bearer-token>"))
      (should (equal messages '("Orgist [WARN] Bad <orgist-bearer-token>")))
      ;; Flushing again finds nothing left.
      (funcall relay nil)
      (should (= (length logged) 4)))))

(ert-deftest orgist-seams/relay-without-a-token-keeps-lines ()
  "With no token, or an empty one, lines are logged as they came."
  (dolist (token '(nil ""))
    (orgist-test--relaying
      (let ((relay (orgist--subprocess-output-relay "orgist-test" token)))
        (funcall relay "Orgist [INFO] Pulled 3 tasks\n")
        (should (equal logged '("orgist-test: Orgist [INFO] Pulled 3 tasks")))
        (should (equal messages '("Orgist: Pulled 3 tasks")))))))

(defmacro orgist-test--with-subprocess-env (&rest body)
  "Run BODY with orgist's files, log and token in a throwaway directory.
Binds LOG to the log file.  Subprocesses get that directory as HOME,
so nothing they save lands in the user's files."
  (declare (indent 0))
  `(let* ((orgist-base-dir (file-name-as-directory (make-temp-file "orgist-run-" t)))
          ;; Expanded while ~ is still the user's home.
          (default-directory (expand-file-name default-directory))
          (load-path (mapcar #'expand-file-name load-path))
          (orgist--directory (expand-file-name orgist--directory))
          (orgist-history-directory nil)
          (orgist-sync-token-filename (expand-file-name "sync_token" orgist-base-dir))
          (orgist-snapshot-file (expand-file-name "snapshots.el" orgist-base-dir))
          (orgist-labels-file (expand-file-name "labels.el" orgist-base-dir))
          (orgist-snapshots nil)
          (orgist-sole-writer nil)
          (log (expand-file-name "orgist.log" orgist-base-dir))
          (orgist-log-file log)
          (orgist-bearer-token orgist-test--token)
          (process-environment (cons (concat "HOME=" orgist-base-dir) process-environment)))
     (unwind-protect (progn ,@body)
       (delete-directory orgist-base-dir t))))

(defun orgist-test--run-subprocess (name job-body)
  "Run subprocess NAME with JOB-BODY, a list of forms, and wait for its sentinel.
The job writes its done file after JOB-BODY.  Returns (PROCESS
. OUTCOME), OUTCOME being (success COUNT) or (failure EVENT)."
  (let* ((outcome nil)
         (done (expand-file-name (concat name "-done") orgist-base-dir))
         (process (orgist--run-subprocess
                   (list :name name
                         :data-files nil
                         :done-file done
                         :needs-http nil
                         :job-body (append job-body
                                           `((with-temp-file ,done (insert "1\n"))))
                         :on-success (lambda (count) (setq outcome (list 'success count)))
                         :on-failure (lambda (event) (setq outcome (list 'failure event))))))
         (deadline (+ (float-time) 120)))
    (while (and (not outcome) (< (float-time) deadline))
      (accept-process-output nil 0.1))
    (cons process outcome)))

(ert-deftest orgist-seams/subprocess-token-comes-on-stdin ()
  "The subprocess has the token, and its command line does not."
  (orgist-test--with-subprocess-env
    (ignore log)
    (dolist (token (list orgist-test--token nil))
      (let* ((orgist-bearer-token token)
             (seen (expand-file-name "token-seen" orgist-base-dir))
             (run (orgist-test--run-subprocess
                   "orgist-test"
                   `((with-temp-file ,seen (prin1 orgist-bearer-token (current-buffer)))))))
        (should (equal (cdr run) '(success 1)))
        (should (equal (orgist-test--file-string seen) (prin1-to-string token)))
        (should-not (string-search orgist-test--token
                                   (string-join (process-command (car run)) " ")))))))

(ert-deftest orgist-seams/subprocess-failure-logs-no-token ()
  "A failing subprocess's output, backtrace included, is logged without the token."
  (orgist-test--with-subprocess-env
    (let ((run (orgist-test--run-subprocess
                "orgist-test"
                '((message "Authorization: Bearer %s" orgist-bearer-token)
                  (error "Request failed with %s" orgist-bearer-token)))))
      (should (eq (car (cdr run)) 'failure))
      (let ((text (orgist-test--file-string log)))
        (should (string-search "Authorization: Bearer <orgist-bearer-token>" text))
        (should-not (string-search orgist-test--token text))))))

(provide 'test-seams)
;;; test-seams.el ends here
