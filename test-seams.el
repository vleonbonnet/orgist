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

(provide 'test-seams)
;;; test-seams.el ends here
