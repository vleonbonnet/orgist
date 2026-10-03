;;; test-sync.el --- Test orgist sync in batch mode -*- lexical-binding: t; -*-

;; Usage: timeout 120 emacs --batch -l test-sync.el -- [PROJECT-NAME]
;; If PROJECT-NAME is provided, only sync that project.
;;
;; The sync runs in a throwaway `orgist-base-dir', never the user's
;; mirror directory: a fresh directory has no snapshots, so no
;; write-back can run, and the pull reads Todoist without changing
;; it.  Pass --keep to leave the directory in place for inspection.

;; Add dependency paths (elpaca builds)
(add-to-list 'load-path
             (expand-file-name "~/.emacs.d/elpaca/builds/request"))
;; org-sync-confirm is required by orgist-confirm; prefer the elpaca build,
;; fall back to the source checkout on a fresh clone.
(dolist (dir '("~/.emacs.d/elpaca/builds/org-sync-confirm"
               "~/.emacs.d/elpaca/sources/org-sync-confirm"))
  (when (file-directory-p (expand-file-name dir))
    (add-to-list 'load-path (expand-file-name dir))))

;; Load orgist from current directory
(add-to-list 'load-path default-directory)
(require 'test-isolation)
(require 'orgist)

;; Set bearer token from env, falling back to `pass`
(setq orgist-bearer-token
      (or (getenv "TODOIST_API_TOKEN")
          (string-trim
           (shell-command-to-string "pass show todoist/api-token 2>/dev/null"))
          (error "No API token: set TODOIST_API_TOKEN or configure `pass`")))

;; Console shows info+warn; log file always gets all levels
(setq orgist-log-level 'info)

;; Check for arguments (skip "--" separator)
(setq command-line-args-left
      (seq-remove (lambda (a) (string= a "--")) command-line-args-left))
;; --full is accepted for compatibility: a fresh directory always syncs fully.
(setq command-line-args-left
      (seq-remove (lambda (a) (string= a "--full")) command-line-args-left))
(defvar orgist-test-keep-dir (member "--keep" command-line-args-left))
(setq command-line-args-left
      (seq-remove (lambda (a) (string= a "--keep")) command-line-args-left))
(let ((project-filter (car command-line-args-left)))
  (setq command-line-args-left (cdr command-line-args-left))
  (when project-filter
    (setq orgist-sync-project-filter project-filter)
    (message "=== Project filter set to: %s ===" project-filter)))

;; Isolate every file orgist reads or writes.
(let ((dir (file-name-as-directory (make-temp-file "orgist-live-sync-" t))))
  (setq orgist-base-dir dir
        orgist-sync-token-filename (concat dir "sync_token")
        orgist-snapshot-file (concat dir "snapshots.el")
        orgist-labels-file (concat dir "labels.el")
        orgist-log-file (concat dir "orgist.log")
        orgist-auto-pull-interval nil
        orgist-sync-completed-tasks nil
        orgist-sync-comments nil))

(message "=== Starting orgist sync test ===")
(message "=== orgist-base-dir: %s ===" orgist-base-dir)
(message "=== orgist-sync-project-filter: %s ===" orgist-sync-project-filter)

(orgist)

;; Wait for async request to complete
(let ((waited 0)
      (max-wait 300))
  (while (and orgist-sync-mutex (< waited max-wait))
    (sleep-for 1)
    (setq waited (1+ waited))
    (when (= (% waited 10) 0)
      (message "=== Waiting for sync... %ds ===" waited)))
  (if (>= waited max-wait)
      (progn
        (message "=== TIMEOUT: sync did not complete in %ds ===" max-wait)
        (kill-emacs 2))
    (message "=== Sync completed in %ds ===" waited)))

;; Show results
(message "=== Files in orgist-base-dir: ===")
(dolist (file (directory-files orgist-base-dir nil "\\.org$"))
  (message "  %s (%d bytes)" file
           (file-attribute-size (file-attributes
                                 (expand-file-name file orgist-base-dir)))))

;; Show log tail
(message "=== Last 30 lines of log: ===")
(when (file-exists-p orgist-log-file)
  (with-temp-buffer
    (insert-file-contents orgist-log-file)
    (let* ((lines (split-string (buffer-string) "\n"))
           (total (length lines))
           (tail-lines (nthcdr (max 0 (- total 30)) lines)))
      (message "  (log has %d total lines)" total)
      (dolist (line tail-lines)
        (message "  %s" line)))))

(if orgist-test-keep-dir
    (message "=== Kept %s ===" orgist-base-dir)
  (dolist (buf (buffer-list))
    (when-let* ((file (buffer-file-name buf)))
      (when (string-prefix-p (expand-file-name orgist-base-dir) (expand-file-name file))
        (with-current-buffer buf (set-buffer-modified-p nil))
        (kill-buffer buf))))
  (delete-directory orgist-base-dir t))

(message "=== Test complete ===")
(kill-emacs 0)
