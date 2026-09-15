;;; test-sync.el --- Test orgist sync in batch mode -*- lexical-binding: t; -*-

;; Usage: timeout 120 emacs --batch -l test-sync.el -- [PROJECT-NAME]
;; If PROJECT-NAME is provided, only sync that project.

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
;; Parse --full flag (forces full sync without deleting saved token)
(defvar orgist-test-force-full nil)
(when (member "--full" command-line-args-left)
  (setq orgist-test-force-full t)
  (setq command-line-args-left
        (seq-remove (lambda (a) (string= a "--full")) command-line-args-left))
  (message "=== Full sync forced (sync token ignored) ==="))
(let ((project-filter (car command-line-args-left)))
  (setq command-line-args-left (cdr command-line-args-left))
  (when project-filter
    (setq orgist-sync-project-filter project-filter)
    (message "=== Project filter set to: %s ===" project-filter)))

(message "=== Starting orgist sync test ===")
(message "=== orgist-base-dir: %s ===" orgist-base-dir)
(message "=== orgist-sync-project-filter: %s ===" orgist-sync-project-filter)


;; Run sync (override sync token when --full is used)
(if orgist-test-force-full
    (let ((orgist-sync-token-filename
           (concat orgist-base-dir "sync_token_NONEXISTENT")))
      (orgist))
  (orgist))

;; Wait for async request to complete (poll for up to 90 seconds)
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

(message "=== Test complete ===")
(kill-emacs 0)
