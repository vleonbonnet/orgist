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

(provide 'test-seams)
;;; test-seams.el ends here
