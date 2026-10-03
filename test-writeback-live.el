;;; test-writeback-live.el --- Live write-back checks on the Orgtest project -*- lexical-binding: t; -*-

;; Usage: ./test-run.sh live-writeback   (needs a token and pandoc)
;;
;; Runs against the real Todoist API, restricted to the Orgtest project,
;; in a throwaway `orgist-base-dir' — never the user's mirror directory.
;; Every task it creates is deleted at the end, also on failure.
;;
;; Identity: a task carrying an org-id before its first push keeps it;
;; the Todoist ID goes to TODOIST_ID and rename and delete go through it.
;; Description: sub-headings travel in the description, an org-id on a
;; sub-heading pushes nothing and survives an unrelated remote edit, and
;; a remote description edit rebuilds the body.

(add-to-list 'load-path
             (expand-file-name "~/.emacs.d/elpaca/builds/request"))
(dolist (dir '("~/.emacs.d/elpaca/builds/org-sync-confirm"
               "~/.emacs.d/elpaca/sources/org-sync-confirm"))
  (when (file-directory-p (expand-file-name dir))
    (add-to-list 'load-path (expand-file-name dir))))
(add-to-list 'load-path default-directory)
(require 'test-isolation)
(require 'orgist)
(require 'cl-lib)
(require 'json)

(defvar orgist-live--failures 0)
(defvar orgist-live--created nil "Todoist IDs of tasks this run created.")

(defun orgist-live--check (ok msg)
  (message "%s %s" (if ok "[PASS]" "[FAIL]") msg)
  (unless ok (cl-incf orgist-live--failures)))

(unless (executable-find "pandoc")
  (error "pandoc not found: the description checks need it"))
(setq orgist-bearer-token (or (getenv "TODOIST_API_TOKEN")
                              (error "TODOIST_API_TOKEN not set")))
(let ((dir (file-name-as-directory (make-temp-file "orgist-live-writeback-" t))))
  (setq orgist-base-dir dir
        orgist-sync-token-filename (concat dir "sync_token")
        orgist-snapshot-file (concat dir "snapshots.el")
        orgist-labels-file (concat dir "labels.el")
        orgist-log-file (concat dir "orgist.log")
        orgist-sync-project-filter "Orgtest"
        orgist-enable-write-back t
        orgist-write-back-dry-run nil
        orgist-sync-comments nil
        orgist-sync-attachments nil
        orgist-sync-metadata nil
        orgist-sync-completed-tasks nil
        orgist-auto-pull-interval nil
        orgist-log-level 'info))

;;; Helpers

(defun orgist-live--pull ()
  (setq orgist-sync-mutex (current-time))
  (orgist-pull)
  (let ((waited 0))
    (while (and orgist-sync-mutex (< waited 240))
      (sleep-for 0.5)
      (cl-incf waited))
    (when orgist-sync-mutex (error "Pull timed out"))))

(defun orgist-live--api (method id &optional fields)
  "Call the REST task endpoint for ID with METHOD and optional FIELDS.
Returns the task alist, `deleted' when it is gone, t for an empty
success, or nil on error."
  (let (result)
    (request (format "https://api.todoist.com/api/v1/tasks/%s" id)
      :type method :sync t :parser 'json-read
      :headers `(("Authorization" . ,(format "Bearer %s" orgist-bearer-token))
                 ("Content-Type" . "application/json"))
      :data (when fields (encode-coding-string (json-encode fields) 'utf-8))
      :success (cl-function
                (lambda (&key data &allow-other-keys)
                  (setq result (cond ((eq (alist-get 'is_deleted data) t) 'deleted)
                                     (data data)
                                     (t t)))))
      :error (cl-function
              (lambda (&key response &allow-other-keys)
                (when (eql (request-response-status-code response) 404)
                  (setq result 'deleted)))))
    result))

(defun orgist-live--buffer ()
  (find-buffer-visiting (expand-file-name "Orgtest.org" orgist-base-dir)))

(defun orgist-live--goto (title)
  "Move to the task heading TITLE (with or without a priority cookie)."
  (goto-char (point-min))
  (when (re-search-forward
         (format "^\\*+ TODO \\(?:\\[#.\\] \\)?%s$" (regexp-quote title)) nil t)
    (org-back-to-heading t)
    (point)))

(defun orgist-live--count (title)
  (with-current-buffer (orgist-live--buffer)
    (save-excursion
      (goto-char (point-min))
      (let ((n 0))
        (while (re-search-forward
                (format "^\\*+ TODO \\(?:\\[#.\\] \\)?%s$" (regexp-quote title)) nil t)
          (cl-incf n))
        n))))

(defun orgist-live--subtree (title)
  (with-current-buffer (orgist-live--buffer)
    (save-excursion
      (when (orgist-live--goto title)
        (buffer-substring-no-properties (point) (orgist--subtree-end))))))

(defun orgist-live--append (text)
  (with-current-buffer (orgist-live--buffer)
    (goto-char (point-max))
    (unless (bolp) (insert "\n"))
    (insert text)
    (orgist-live--save)))

(defun orgist-live--save ()
  (let ((orgist--inhibit-after-save t))
    (save-buffer)))

(defun orgist-live--pending (id)
  (with-current-buffer (orgist-live--buffer)
    (alist-get id (orgist-diff-all-elements) nil nil #'equal)))

(defun orgist-live--id-of (title)
  (with-current-buffer (orgist-live--buffer)
    (save-excursion
      (when (orgist-live--goto title)
        (let ((id (orgist--element-id)))
          (when (and id (not (orgist--temp-id-p id)))
            (push id orgist-live--created))
          id)))))

;;; Scenarios

(defun orgist-live--identity (stamp)
  (let* ((title (format "orgist live identity %s" stamp))
         (renamed (concat title " renamed"))
         (org-id (org-id-uuid))
         id)
    (message "--- Identity ---")
    (orgist-live--append (concat "* TODO " title "\n:PROPERTIES:\n:ID:       " org-id "\n:END:\n"))
    (orgist-write-back)
    (setq id (orgist-live--id-of title))
    (with-current-buffer (orgist-live--buffer)
      (orgist-live--goto title)
      (orgist-live--check (equal (org-entry-get (point) "ID") org-id)
                          "org-id unchanged after item_add")
      (orgist-live--check (equal (org-entry-get (point) "TODOIST_ID") id)
                          "Todoist ID in TODOIST_ID"))
    (orgist-live--check (and id (not (orgist--temp-id-p id))) "real Todoist ID assigned")
    (let ((task (and id (orgist-live--api "GET" id))))
      (orgist-live--check (and (consp task) (equal (alist-get 'content task) title))
                          "task exists in Todoist"))
    (orgist-live--pull)
    (orgist-live--check (= (orgist-live--count title) 1) "re-pull leaves one heading")
    (with-current-buffer (orgist-live--buffer)
      (orgist-live--goto title)
      (orgist-live--check (equal (org-entry-get (point) "ID") org-id) "org-id survives re-pull")
      (org-edit-headline renamed)
      (orgist-live--save))
    (orgist-write-back)
    (let ((task (orgist-live--api "GET" id)))
      (orgist-live--check (and (consp task) (equal (alist-get 'content task) renamed))
                          "rename pushed through TODOIST_ID"))
    (with-current-buffer (orgist-live--buffer)
      (when (orgist-live--goto renamed)
        (orgist-delete-subtree))
      (orgist-live--save))
    (orgist-write-back)
    (orgist-live--check (eq (orgist-live--api "GET" id) 'deleted)
                        "local deletion pushed as item_delete")))

(defun orgist-live--description (stamp)
  (let* ((title (format "orgist live description %s" stamp))
         (org-id (org-id-uuid))
         id)
    (message "--- Description ---")
    (orgist-live--append (concat "* TODO " title "\n\nIntro line.\n\n** Notes\n\nInitial note.\n"))
    (orgist-write-back)
    (setq id (orgist-live--id-of title))
    (let ((desc (alist-get 'description (orgist-live--api "GET" id))))
      (orgist-live--check (and desc (string-match-p "^# Notes$" desc)
                               (string-match-p "Initial note" desc))
                          "item_add carries the sub-heading"))
    (with-current-buffer (orgist-live--buffer)
      (goto-char (point-min))
      (re-search-forward "^Initial note\\.$")
      (replace-match "Edited note.")
      (orgist-live--save))
    (orgist-write-back)
    (let ((desc (alist-get 'description (orgist-live--api "GET" id))))
      (orgist-live--check (and desc (string-match-p "Edited note" desc)
                               (string-match-p "Intro line" desc)
                               (string-match-p "^# Notes$" desc))
                          "edit below the sub-heading pushed, nothing truncated"))
    (orgist-live--check (null (orgist-live--pending id)) "nothing pending after the push")
    (with-current-buffer (orgist-live--buffer)
      (goto-char (point-min))
      (re-search-forward "^\\*\\* Notes$")
      (org-entry-put (point) "ID" org-id)
      (orgist-live--save))
    (orgist-live--check (null (orgist-live--pending id)) "org-id on a sub-heading pushes nothing")
    (orgist-write-back)
    (orgist-live--check (orgist-live--api "POST" id '((priority . 3))) "remote priority change sent")
    (orgist-live--pull)
    (let ((sub (orgist-live--subtree title)))
      (orgist-live--check (and sub (string-match-p (regexp-quote org-id) sub))
                          "unchanged description keeps the sub-heading's org-id")
      (orgist-live--check (and sub (string-match-p "\\[#" sub)) "priority change applied"))
    (let ((pending (orgist-live--pending id)))
      (orgist-live--check (null pending) (format "nothing pending after the pull %S" pending)))
    (orgist-live--check (orgist-live--api "POST" id
                                          '((description . "Intro line.\n\n# Notes\n\nRemote note.")))
                        "remote description change sent")
    (orgist-live--pull)
    (let ((sub (orgist-live--subtree title)))
      (orgist-live--check (and sub (string-match-p "Remote note\\." sub)
                               (not (string-match-p "Edited note" sub)))
                          "changed description replaces the body")
      (orgist-live--check (and sub (string-match-p "^\\*\\* Notes$" sub))
                          "sub-heading rebuilt from the Markdown heading"))
    (orgist-live--check (null (orgist-live--pending id)) "nothing pending after the replacement")
    (let ((journal (with-temp-buffer
                     (when (file-exists-p (orgist--journal-file))
                       (insert-file-contents (orgist--journal-file)))
                     (buffer-string))))
      (orgist-live--check (string-match-p (concat "Replaced description of " (regexp-quote title))
                                          journal)
                          "the replaced description is in the journal")
      (orgist-live--check (string-match-p "Edited note\\." journal)
                          "the journal holds the replaced text verbatim"))))

(defun orgist-live--history ()
  (message "--- History ---")
  (let ((subjects (mapcar #'caddr (org-sync-safety-history-log (orgist--history)))))
    (orgist-live--check (member "Before pull: local state" subjects) "pulls record the state they start from")
    (orgist-live--check (seq-find (lambda (s) (string-prefix-p "Pull: " s)) subjects)
                        "pulls record their result")
    (orgist-live--check (member "Before write-back: local edits" subjects)
                        "write-backs record the local edits they push")
    (orgist-live--check (seq-find (lambda (s) (string-prefix-p "Write-back: " s)) subjects)
                        "write-backs record their result")))

;;; Run

(let ((stamp (format-time-string "%Y%m%d%H%M%S")))
  (unwind-protect
      (progn
        (orgist-live--pull)
        (orgist-live--check (orgist-live--buffer) "initial pull created Orgtest.org")
        (orgist-live--identity stamp)
        (orgist-live--description stamp)
        (orgist-live--history))
    (dolist (id orgist-live--created)
      (when (consp (orgist-live--api "GET" id))
        (message "cleanup: deleting task %s" id)
        (orgist-live--api "DELETE" id)))
    (dolist (buf (buffer-list))
      (when-let* ((file (buffer-file-name buf)))
        (when (string-prefix-p (expand-file-name orgist-base-dir) (expand-file-name file))
          (with-current-buffer buf (set-buffer-modified-p nil))
          (kill-buffer buf))))
    (delete-directory orgist-base-dir t)))

(message "=== Live write-back: %d failure(s) ===" orgist-live--failures)
(kill-emacs (if (zerop orgist-live--failures) 0 1))
