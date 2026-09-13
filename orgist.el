;;; orgist.el --- Sync org-mode with Todoist  -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2026 Valentin Leon

;; Author: Valentin Leon <valentin@leon.click>
;; Created: 18 Jun 2023
;; Version: 0.1
;; Keywords: productivity
;; URL: https://citadel.leon.click/
;; Package-Requires: ((org) (request) (json))

;; This file is not part of GNU Emacs.

;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary

;; This library syncs an org-mode file to a Todoist project using the
;; sync API. It makes a call to https://api.todoist.com/api/v1/sync
;; to get the list of projects, then look at the org global property
;; "orgist-project" to know which project this file is
;; associated with using: (assoc-string "orgist-project"
;; org-keyword-properties t) Once it retrieved the project id, it
;; updates all tasks in the file. Each task is mapped to an org
;; header.

;;; Change Log

;; v0.1 - Initial version.

;;; Code:

(defvar orgist--directory (file-name-directory
                           (or load-file-name buffer-file-name))
  "Directory where orgist.el lives, captured at load time.")

;;; Dependencies
(require 'cl-lib)
(require 'subr-x)
(require 'org)
(require 'request)
(require 'json)
(require 'calendar)
(require 'org-attach)
(require 'orgist-confirm)

;; Loaded lazily; call site is guarded by `featurep'.
(declare-function org-inlinetask-outline-regexp "org-inlinetask")

;;; Settings
(defgroup orgist nil
  "Bi-directional sync between Todoist and Org Mode."
  :group 'org)

(defcustom orgist-base-dir
  (concat user-emacs-directory "orgist/")
  "Directory in which orgist will save projects."
  :group 'orgist
  :type 'string)

(defcustom orgist-tag "todoist"
  "Tag added to all files created by orgist."
  :group 'orgist
  :type 'string)

(defcustom orgist-sync-token-filename
  (concat orgist-base-dir "sync_token")
  "Filename to save the sync token for the Todoist API."
  :group 'orgist
  :type 'string)

(defcustom orgist-bearer-token nil
  "Bearer token for Todoist API."
  :group 'orgist
  :type 'string)

(defcustom orgist-treat-priority-4-as-none t
  "When non-nil, treat Todoist lowest priority as no priority in org-mode."
  :group 'orgist
  :type 'boolean)

(defcustom orgist-log-level 'info
  "Logging level for orgist operations."
  :type '(choice (const :tag "Debug" debug)
                 (const :tag "Info" info)
                 (const :tag "Warning" warn)))

(defcustom orgist-log-file
  (concat orgist-base-dir "orgist.log")
  "File to write sync logs to.  Set to nil to disable file logging."
  :group 'orgist
  :type '(choice string (const nil)))

(defcustom orgist-log-max-bytes (* 100 1024 1024)
  "Rotate `orgist-log-file' when it exceeds this many bytes.
On rotation the file is renamed to \"<name>.old\" (replacing any
previous one), so up to two generations are kept.  Set to nil to
never rotate."
  :group 'orgist
  :type '(choice integer (const :tag "Never rotate" nil)))

(defcustom orgist-sync-project-filter nil
  "When non-nil, only sync this project (by name).
Useful for debugging.  Set to a project name string to limit sync
to that project, or nil to sync all projects."
  :group 'orgist
  :type '(choice string (const nil)))

(defcustom orgist-enable-write-back 'ask
  "Control write-back behavior for local changes.
nil     — Disable write-back entirely.
t       — Write back without confirmation.
`ask'   — Show a confirmation buffer before writing (default).
The `ask' confirmation buffer (`orgist-confirm-mode') is the normal
guard against unwanted sends; `orgist-write-back-dry-run' remains
available as an additional safety net that logs commands without
sending them."
  :group 'orgist
  :type '(choice (const :tag "Off" nil)
                 (const :tag "On (no confirmation)" t)
                 (const :tag "Ask (show confirmation buffer)" ask)))

(defcustom orgist-write-back-dry-run nil
  "When non-nil, log write-back commands without sending them.
This is a safety net: even when `orgist-enable-write-back' is t,
dry-run mode prevents actual API calls."
  :group 'orgist
  :type 'boolean)

(defcustom orgist-sync-on-save t
  "When non-nil, trigger write-back automatically when saving an orgist buffer.
Uses `after-save-hook' so that `C-x C-s' pushes local changes.
Respects `orgist-enable-write-back' and `orgist-write-back-dry-run'."
  :group 'orgist
  :type 'boolean)

(defcustom orgist-auto-pull-interval 300
  "Seconds between automatic pulls from Todoist, or nil to disable.
When set, orgist periodically pulls changes on a timer and also
pulls when switching to an orgist buffer if the interval has
elapsed.  Auto-pull is read-only — it never triggers write-back."
  :group 'orgist
  :type '(choice (integer :tag "Seconds")
                 (const :tag "Disabled" nil)))

(defcustom orgist-snapshot-file
  (concat orgist-base-dir "snapshots.el")
  "File to persist element snapshots between sessions.
Snapshots record each element's state after the last sync from
Todoist, enabling change detection for write-back."
  :group 'orgist
  :type 'string)

(defcustom orgist-labels-file
  (concat orgist-base-dir "labels.el")
  "File to persist label definitions between sessions.
Without this, `orgist-labels' starts nil after an Emacs restart.
Incremental sync returns an empty labels array when nothing changed,
so the cache would stay nil and every existing tag would generate a
spurious label_add on write-back."
  :group 'orgist
  :type 'string)

(defcustom orgist-sync-comments nil
  "When non-nil, pull Todoist comments and activity during sync.
After a normal sync completes, orgist will fetch per-task comments
and activity from the Todoist API and insert them as logbook entries.
This runs in a subprocess to avoid blocking the UI."
  :group 'orgist
  :type 'boolean)

(defcustom orgist-sync-attachments nil
  "When non-nil, sync file attachments between Todoist and org-attach.
Pull: files from Todoist comment attachments are downloaded into the
heading's org-attach directory.  Push: new local files generate
upload + comment commands; removed files generate comment deletions.
Requires `orgist-sync-comments' to be non-nil (attachments live on
comments in Todoist)."
  :group 'orgist
  :type 'boolean)

(defcustom orgist-sync-metadata nil
  "When non-nil, preserve non-Todoist org properties via a special comment.
Properties and custom drawers from other tools (e.g., org-gcal) are
stored as a Todoist comment with marker [orgist-metadata] so they
survive sync and can be restored on another computer.
Requires `orgist-sync-comments' to be non-nil."
  :group 'orgist
  :type 'boolean)

(defconst orgist--managed-properties
  '("ID" "TODOIST-PROJECT" "TODOIST-ORDER" "TODOIST_DUE_STRING"
    "SECTION" "ASSIGNEE" "REMINDER-LOC" "CREATED" "LAST_REPEAT"
    "CATEGORY" "ATTACH_DIR" "DIR" "ITEM")
  "Property names managed by orgist or org-mode internals.
These are excluded from metadata preservation.")

(defcustom orgist-sync-completed-tasks nil
  "When non-nil, pull completed (archived) tasks from Todoist.
After sync, fetches completed tasks and inserts them as DONE
headings.  Runs in a subprocess to avoid blocking the UI.
Requires a Todoist plan that supports completed tasks."
  :group 'orgist
  :type 'boolean)

(defcustom orgist-completed-tasks-since-days 30
  "Days back to look for completed tasks on first pull.
On subsequent pulls, only tasks completed since the last
pull are fetched."
  :group 'orgist
  :type 'integer)

(defcustom orgist-completed-retry-attempts 10
  "How many pulls to retry a completed task that could not be placed.
A completed task whose project buffer or parent cannot be resolved is
persisted and retried on later pulls; after this many attempts it is
dropped with a warning (tasks of archived projects are never placeable)."
  :group 'orgist
  :type 'integer)

(defcustom orgist-browse-url-format nil
  "URL template for opening Todoist tasks, or nil for auto-detect.
%s is replaced with the task ID.
When nil, uses \"todoist://task?id=%%s\" if the desktop app is
found, otherwise \"https://app.todoist.com/app/task/%%s\"."
  :group 'orgist
  :type '(choice (const :tag "Auto-detect" nil)
                 (string :tag "URL template")))

;;; Local vars
(defvar orgist-project-buffer-cache nil
  "Cache mapping Todoist project IDs to their corresponding buffers.
Each entry is of the form (PROJECT-ID . BUFFER).")

(defvar orgist-sync-mutex nil
  "Mutex for sync operations.
When active, holds the timestamp at which it was set.
Considered stale after `orgist-sync-mutex-timeout' seconds.")

(defvar orgist-sync-mutex-timeout 120
  "Seconds after which `orgist-sync-mutex' is considered stale.
If a sync operation has held the mutex for longer than this,
the after-save hook treats it as cleared and logs a warning.")

(defvar orgist--pull-counts nil
  "Counts from the latest sync response, for summary logging.
A list (PROJECTS SECTIONS ITEMS) or nil before the first pull.")

(defvar orgist-plan-limits nil
  "Cached user plan limits from the Todoist Sync API.
An alist with keys like `activity_log' (boolean) and
`activity_log_limit' (integer, days of history).
Fetched once per session by `orgist-fetch-plan-limits'.")

(defvar orgist-user-timezone nil
  "User's IANA timezone from the Todoist user profile.
Populated by `orgist-store-user-profile' when the `user'
resource is included in the sync response.  Used to convert
Todoist timestamps with explicit timezones to local time.")

(defvar orgist-user-inbox-project-id nil
  "The user's inbox project ID from the Todoist user profile.")

(defvar orgist-labels nil
  "Hash table mapping Todoist label IDs to label definitions.
Each value is an alist with keys `name', `color', `order'.")

(defvar orgist-reminders nil
  "Hash table mapping Todoist item IDs to lists of reminder objects.")

(defvar orgist-collaborators nil
  "Hash table mapping user IDs to collaborator profiles.
Each value is an alist with keys `full_name', `email'.")

(defvar-local orgist-id-cache nil
  "Buffer-local hash table mapping element IDs to markers.")

(defvar orgist-snapshots nil
  "Hash table mapping element IDs to their last-synced state.
Each value is a plist with keys :content, :checked, :priority,
:labels, :due, :deadline, :description, :parent-id, :order,
:section-p.  Populated by `orgist-snapshot-element' after sync.")

(defvar orgist--auto-pull-timer nil
  "Timer for periodic auto-pull, or nil when not running.")

(defvar orgist--auto-pull-deferred nil
  "Non-nil while auto-pull is deferred by pending local changes.
Used to log the deferral at info level once (then at debug on
repeats) and to log when pulls resume.")

(defvar orgist--write-back-stamps nil
  "Hash table mapping project file names to verified content hashes.
See `orgist--stamps-path'.  Loaded lazily by `orgist--load-stamps'.")

(defvar orgist--write-back-stamps-path nil
  "Path `orgist--write-back-stamps' was last loaded from.
When `orgist--stamps-path' returns a different location (e.g. the
test harness switched runtime directories), the table is reloaded
so stamps never leak across data directories.")

(defvar orgist--pending-stamps nil
  "Alist of (FILE . HASH) captured by the last diff scan.
Holds the scan-time hash of files whose detected changes are
awaiting execution or confirmation.  Committed to
`orgist--write-back-stamps' by `orgist--commit-pending-stamps'
after all commands succeed; simply dropped on failure or cancel so
the files stay due for scanning.  Hashes are captured at scan time
on purpose: edits made after the scan produce a different hash, so
committing the scan-time value can never mask them.")

(defvar orgist--last-pull-time nil
  "Time of the last successful pull (from `current-time').
Used by auto-pull to avoid pulling more often than `orgist-auto-pull-interval'.")

(defvar orgist--batch-save-pending nil
  "When non-nil, a hash-set of buffers with unsaved changes.")

(defvar orgist--skip-initial-sync nil
  "When non-nil, `orgist-mode' sets up hooks but does not trigger a sync.
Used by `maybe-enable-orgist' when re-enabling after a buffer revert.")

(defvar orgist--inhibit-after-save nil
  "When non-nil, `orgist--after-save' does nothing.
Used to prevent cascading write-back triggers when saving
multiple orgist buffers before a diff scan.")

(defvar orgist--inhibit-sibling-order-update nil
  "When non-nil, `orgist--update-sibling-orders' does nothing.
Bound during programmatic moves performed by orgist itself
(e.g. `orgist-position-element-by-order' during inbound sync) so
that those moves don't renumber every sibling's TODOIST-ORDER and
trigger a flood of spurious item_reorder write-backs.")

(defvar orgist--log-buffer nil
  "When non-nil, a list accumulator for deferred log writes.")

(defvar orgist--pandoc-lua-filter nil
  "Path to a temporary Lua filter for pandoc Unicode preservation.
Created lazily by `orgist--pandoc-lua-filter'.")

(defun orgist--save-buffer ()
  "Save current buffer, or defer if batch mode is active."
  (if orgist--batch-save-pending
      (puthash (current-buffer) t orgist--batch-save-pending)
    (save-buffer)))

(defun orgist--flush-pending-saves ()
  "Save all buffers accumulated during batch mode."
  (when orgist--batch-save-pending
    (maphash (lambda (buf _)
               (when (buffer-live-p buf)
                 (with-current-buffer buf
                   (when (buffer-modified-p)
                     (save-buffer)))))
             orgist--batch-save-pending)))

(defvar orgist--log-writes-since-rotate-check 0
  "File-log writes since the last rotation size check.")

(defun orgist--maybe-rotate-log ()
  "Rename `orgist-log-file' to \"<name>.old\" when it exceeds
`orgist-log-max-bytes'.  Throttled to one size check per 500 writes."
  (when (and orgist-log-max-bytes orgist-log-file)
    (setq orgist--log-writes-since-rotate-check
          (1+ orgist--log-writes-since-rotate-check))
    (when (>= orgist--log-writes-since-rotate-check 500)
      (setq orgist--log-writes-since-rotate-check 0)
      (when-let* ((attrs (file-attributes orgist-log-file))
                  (size (file-attribute-size attrs)))
        (when (> size orgist-log-max-bytes)
          (let ((old (concat orgist-log-file ".old")))
            (when (file-exists-p old) (delete-file old))
            (rename-file orgist-log-file old)
            (orgist-log 'info "Rotated log to %s (%d bytes)" old size)))))))

(defun orgist--flush-log-buffer ()
  "Flush accumulated log messages to `orgist-log-file'."
  (when (and orgist--log-buffer orgist-log-file)
    (orgist--maybe-rotate-log)
    (let ((log-dir (file-name-directory orgist-log-file))
          (coding-system-for-write 'utf-8-unix))
      (unless (file-directory-p log-dir)
        (make-directory log-dir t))
      (write-region (string-join (nreverse orgist--log-buffer) "")
                    nil orgist-log-file t 'silent))
    (setq orgist--log-buffer nil)))

;;; Auto-pull

(defun orgist--auto-pull-due-p ()
  "Return non-nil if enough time has elapsed since the last pull."
  (or (null orgist--last-pull-time)
      (> (float-time (time-subtract (current-time) orgist--last-pull-time))
         orgist-auto-pull-interval)))

(defun orgist--any-orgist-buffer-p ()
  "Return non-nil if any live buffer has `orgist-mode' enabled."
  (cl-some (lambda (b)
             (and (buffer-live-p b)
                  (buffer-local-value 'orgist-mode b)))
           (buffer-list)))

(defun orgist--pending-local-changes ()
  "Return pending local write-back changes, or nil.
Runs the write-back diff scan without generating commands or
showing any UI.  As a side effect the scan advances verification
stamps for files that prove clean, so repeated calls are cheap
when nothing changed.  Returns nil when write-back is disabled or
no snapshots exist yet (first sync)."
  (when orgist-enable-write-back
    (orgist-load-snapshots)
    (when (> (hash-table-count orgist-snapshots) 0)
      (orgist-diff-all-elements))))

(defun orgist--auto-pull ()
  "Pull from Todoist if not already syncing.
Sets `orgist-sync-mutex' and calls `orgist-pull' directly — no
write-back commands are sent and no confirmation UI is shown.
When the diff scan finds pending local changes the pull is
skipped instead: applying remote updates over unpushed local
edits could overwrite them and refresh their snapshots, losing
the changes permanently.  The user pushes them via a save or
\\[orgist]; pulls resume once clean."
  (when (and orgist-bearer-token
             (not orgist-sync-mutex)
             orgist-auto-pull-interval)
    (let ((pending (condition-case err
                       (orgist--pending-local-changes)
                     (error
                      (orgist-log 'warn "Auto-pull change detection failed: %s"
                                  (error-message-string err))
                      ;; Fail safe: treat as pending so we don't pull
                      ;; over changes we could not inspect.
                      'error))))
      (if pending
          (if orgist--auto-pull-deferred
              (orgist-log 'debug "Auto-pull still deferred: local changes pending write-back")
            (setq orgist--auto-pull-deferred t)
            (orgist-log 'info "Auto-pull deferred: local changes pending write-back — save the file or M-x orgist to push them"))
        (when orgist--auto-pull-deferred
          (setq orgist--auto-pull-deferred nil)
          (orgist-log 'debug "Auto-pull resumed: no more pending local changes"))
        (setq orgist--last-pull-time (current-time))
        (setq orgist-sync-mutex (current-time))
        (let ((orgist--log-buffer '()))
          (unwind-protect
              (progn
                (orgist-log 'debug "Auto-pull triggered")
                (orgist-pull))
            (orgist--flush-log-buffer)))))))

(defun orgist--auto-pull-timer-fn ()
  "Timer callback for periodic auto-pull."
  (when (and orgist-auto-pull-interval
             (orgist--any-orgist-buffer-p))
    (orgist--auto-pull)))

(defun orgist--on-window-buffer-change (frame)
  "Auto-pull when switching to an orgist buffer.
Added to `window-buffer-change-functions' by `orgist--start-auto-pull'."
  (let ((buf (window-buffer (frame-selected-window frame))))
    (when (and orgist-auto-pull-interval
               (buffer-local-value 'orgist-mode buf)
               (not orgist-sync-mutex)
               (orgist--auto-pull-due-p))
      (orgist--auto-pull))))

(defun orgist--start-auto-pull ()
  "Set up auto-pull timer and buffer-switch hook."
  (when (and orgist-auto-pull-interval (not orgist--auto-pull-timer))
    (setq orgist--auto-pull-timer
          (run-with-timer orgist-auto-pull-interval
                          orgist-auto-pull-interval
                          #'orgist--auto-pull-timer-fn))
    (add-hook 'window-buffer-change-functions #'orgist--on-window-buffer-change)))

(defun orgist--stop-auto-pull ()
  "Tear down auto-pull timer and buffer-switch hook."
  (when orgist--auto-pull-timer
    (cancel-timer orgist--auto-pull-timer)
    (setq orgist--auto-pull-timer nil))
  (remove-hook 'window-buffer-change-functions #'orgist--on-window-buffer-change))

;;; Browse task

(defvar orgist--todoist-app-detected 'unknown
  "Cached result of desktop app detection: t, nil, or `unknown'.")

(defun orgist--todoist-app-installed-p ()
  "Return non-nil if the Todoist desktop app is installed."
  (when (eq orgist--todoist-app-detected 'unknown)
    (setq orgist--todoist-app-detected
          (cond
           ((eq system-type 'windows-nt)
            (let ((appdata (getenv "LOCALAPPDATA")))
              (and appdata
                   (file-exists-p
                    (expand-file-name "Programs/todoist/Todoist.exe" appdata)))))
           ((eq system-type 'darwin)
            (file-exists-p "/Applications/Todoist.app"))
           (t (executable-find "todoist")))))
  orgist--todoist-app-detected)

(defun orgist--browse-url-format ()
  "Return the effective URL format for opening Todoist tasks."
  (or orgist-browse-url-format
      (if (orgist--todoist-app-installed-p)
          "todoist://task?id=%s"
        "https://app.todoist.com/app/task/%s")))

(defun orgist-browse-task ()
  "Open the Todoist task at point.
Uses `orgist-browse-url-format' (or auto-detects desktop app) to
build the URL from the heading's :ID: property."
  (interactive)
  (unless (org-at-heading-p)
    (user-error "Not on an org heading"))
  (let ((id (org-entry-get nil "ID")))
    (unless (and id (not (string-match-p "-" id)))
      (user-error "No Todoist ID on this heading"))
    (browse-url (format (orgist--browse-url-format) id))))

(defun orgist--open-at-point ()
  "Open Todoist task when `org-open-at-point' is called on a heading.
Returns non-nil if handled, nil otherwise.  Intended for
`org-open-at-point-functions'."
  (when (and orgist-mode
             (org-at-heading-p)
             (let ((id (org-entry-get nil "ID")))
               (and id (not (string-match-p "-" id)))))
    (orgist-browse-task)
    t))

;;; Minor Mode
(define-minor-mode orgist-mode
  "Toggle orgist minor mode."
  :lighter "orgist"
  (if orgist-bearer-token
      (if orgist-mode
          (progn
            ;; Always set up the after-save hook — the hook handler
            ;; itself checks orgist-sync-mutex to avoid re-entrance.
            (when orgist-sync-on-save
              (add-hook 'after-save-hook #'orgist--after-save nil t))
            (add-hook 'org-open-at-point-functions #'orgist--open-at-point nil t)
            ;; Order tracking: update TODOIST-ORDER when user moves headings
            (advice-add 'org-move-subtree-down :after #'orgist--after-move-subtree)
            (advice-add 'org-move-subtree-up :after #'orgist--after-move-subtree)
            (add-hook 'org-after-refile-insert-hook #'orgist--on-refile-insert nil t)
            (add-hook 'org-after-sorting-entries-or-items-hook #'orgist--on-sort nil t)
            ;; Start the auto-pull timer unconditionally — it guards
            ;; against double-init internally.  Must live outside the
            ;; unless clause because orgist-sync-mutex is always t
            ;; when buffers are first opened during a sync.
            (orgist--start-auto-pull)
            ;; During sync, files are opened and the hook fires — just
            ;; enable the mode silently (skip logging and initial sync).
            ;; Also skip when re-enabling after a revert (skip-initial-sync).
            (unless (or orgist-sync-mutex orgist--skip-initial-sync)
              (orgist-log 'info "orgist enabled")
              (orgist)))
        (remove-hook 'after-save-hook #'orgist--after-save t)
        (remove-hook 'org-open-at-point-functions #'orgist--open-at-point t)
        (advice-remove 'org-move-subtree-down #'orgist--after-move-subtree)
        (advice-remove 'org-move-subtree-up #'orgist--after-move-subtree)
        (remove-hook 'org-after-refile-insert-hook #'orgist--on-refile-insert t)
        (remove-hook 'org-after-sorting-entries-or-items-hook #'orgist--on-sort t)
        (unless orgist-sync-mutex
          (orgist-log 'info "orgist disabled"))
        (unless (orgist--any-orgist-buffer-p)
          (orgist--stop-auto-pull)))
    (orgist-log 'info "Please set orgist-bearer-token.")
    (setq orgist-mode nil)))

(defun orgist--update-sibling-orders ()
  "Recompute TODOIST-ORDER for all same-type siblings of the heading at point.
Called from org move/refile hooks to keep TODOIST-ORDER in sync
with buffer position, enabling write-back to detect reorders."
  (when (and orgist-mode
             (not orgist--inhibit-sibling-order-update)
             (org-at-heading-p)
             (org-entry-get (point) "TODOIST-ORDER"))
    (orgist--renumber-siblings-at-point)))

(defun orgist--renumber-siblings-at-point ()
  "Number the same-type siblings of the heading at point 0..n by buffer position.
Only siblings carrying both :ID: and :TODOIST-ORDER: take part
\(see `orgist--collect-direct-children').  Returns an alist of
\(ID . NEW-ORDER) for the siblings whose stored order changed, the
heading at point excluded, so a caller that runs after the snapshot
diff can re-diff exactly those."
  (let* ((my-id (org-entry-get (point) "ID"))
         (is-section (not (null (org-entry-get (point) "SECTION"))))
         (parent-level (save-excursion
                         (if (org-up-heading-safe) (org-current-level) 0)))
         (children (save-excursion
                     (if (> parent-level 0)
                         (progn (org-up-heading-safe)
                                (orgist--collect-direct-children parent-level))
                       (goto-char (point-min))
                       (orgist--collect-direct-children 0))))
         ;; Filter to same type
         (siblings (seq-filter
                    (lambda (c) (eq (nth 2 c) is-section))
                    children))
         (changed '())
         (idx 0)
         (todo '()))
    ;; Assign sequential order values based on buffer position.
    (dolist (sib siblings)
      (unless (= (nth 1 sib) idx)
        (push (cons sib idx) todo)
        (unless (equal (car sib) my-id)
          (push (cons (car sib) idx) changed)))
      (setq idx (1+ idx)))
    ;; Write from the last sibling backwards: the positions were captured
    ;; before any edit, and rewriting an earlier sibling's property value
    ;; \(e.g. "9" -> "10") would shift every later position off its
    ;; heading.  TODO was built by `push', so it is already in reverse
    ;; buffer order.
    (dolist (entry todo)
      (save-excursion
        (goto-char (nth 3 (car entry)))
        (org-set-property "TODOIST-ORDER" (number-to-string (cdr entry)))))
    (nreverse changed)))

(defun orgist--assign-new-heading-order ()
  "Give the new task heading at point a TODOIST-ORDER matching its position.
Numbers it and its same-type siblings by buffer position so the
item_add can carry a child_order: without one Todoist appends the
task after its siblings and the next pull drags the heading to the
bottom of its parent, undoing where the user wrote it.  Returns the
\(ID . NEW-ORDER) alist of siblings whose order changed."
  (org-entry-put (point) "TODOIST-ORDER" "0")
  (orgist--renumber-siblings-at-point))

(defun orgist--merge-sibling-order-changes (shifted file-changes)
  "Re-diff the siblings in SHIFTED and merge the results into FILE-CHANGES.
SHIFTED is the (ID . NEW-ORDER) alist from
`orgist--renumber-siblings-at-point'.  The new-heading scan runs
after the snapshot diff loop, so an order it changes on a known
sibling would otherwise go unnoticed until that file is edited
again.  Siblings without a snapshot (other new headings) need no
entry: their item_add carries the order.  Returns the updated
FILE-CHANGES alist."
  (dolist (entry shifted)
    (let ((sid (car entry)))
      (when (gethash sid orgist-snapshots)
        (let ((diff (save-excursion (orgist-diff-element sid))))
          (setq file-changes (assoc-delete-all sid file-changes))
          (when diff
            (push (cons sid diff) file-changes))))))
  file-changes)

(defun orgist--after-move-subtree (&rest _)
  "Advice for `org-move-subtree-down'/`up' to update TODOIST-ORDER.
Recomputes order for all siblings after a subtree move."
  (when (and orgist-mode (org-at-heading-p))
    (orgist--update-sibling-orders)))

(defun orgist--on-refile-insert ()
  "Hook for `org-after-refile-insert-hook' to update TODOIST-ORDER."
  (when orgist-mode
    (orgist--update-sibling-orders)))

(defun orgist--on-sort ()
  "Hook for `org-after-sorting-entries-or-items-hook'."
  (when orgist-mode
    (orgist--update-sibling-orders)))

(defun orgist--save-other-orgist-buffers ()
  "Save all other modified orgist buffers silently.
Used before a diff scan to ensure all project files are flushed
to disk, e.g. after a refile that touches two projects."
  (dolist (buf (buffer-list))
    (when (and (not (eq buf (current-buffer)))
               (buffer-live-p buf)
               (buffer-modified-p buf)
               (buffer-local-value 'orgist-mode buf))
      (with-current-buffer buf
        (save-buffer)))))

(defun orgist--mutex-stale-p ()
  "Return non-nil if `orgist-sync-mutex' has been held too long.
The mutex is stale when it has been active for more than
`orgist-sync-mutex-timeout' seconds.  Also returns non-nil if the
mutex holds a non-timestamp value (e.g. legacy boolean t)."
  (and orgist-sync-mutex
       (or (not (listp orgist-sync-mutex))   ; legacy boolean t
           (> (float-time (time-subtract (current-time) orgist-sync-mutex))
              orgist-sync-mutex-timeout))))

(defun orgist--after-save ()
  "Run write-back after saving an orgist-managed buffer.
Added to `after-save-hook' buffer-locally when `orgist-sync-on-save' is non-nil.
Saves all other modified orgist buffers first so the diff scan
sees consistent on-disk state (important after refile across projects).
Write-back is deferred to the next command loop iteration so that
enclosing batch-save commands (e.g. `org-save-all-org-buffers')
finish before orgist displays its output."
  (when (and orgist-mode orgist-sync-on-save
             (not orgist--inhibit-after-save))
    (cond
     ((not orgist-sync-mutex)
      ;; Normal path — no sync in progress.
      (let ((orgist--inhibit-after-save t))
        (orgist--save-other-orgist-buffers))
      (run-with-timer 0 nil #'orgist--deferred-write-back))
     ((orgist--mutex-stale-p)
      ;; Mutex stuck from a previous operation — clear it and proceed.
      (orgist-log 'warn "Sync mutex stale (held >%ds), clearing"
                  orgist-sync-mutex-timeout)
      (setq orgist-sync-mutex nil)
      (let ((orgist--inhibit-after-save t))
        (orgist--save-other-orgist-buffers))
      (run-with-timer 0 nil #'orgist--deferred-write-back))
     (t
      ;; Active sync — skip, but inform the user.
      (orgist-log 'debug "Write-back skipped: sync in progress")))))

(defun orgist--deferred-write-back ()
  "Run write-back unless a sync is in progress.
Called from a zero-delay timer so it executes after the current
command (and its messages) have completed.  Errors are caught and
logged loudly instead of dying as an opaque timer error: stamps
only advance on a completed scan, so the failed files re-detect
on the next save or sync automatically."
  (unless orgist-sync-mutex
    (condition-case err
        (orgist-write-back)
      (error
       (orgist-log 'error "Write-back failed: %s — changes remain pending and retry on the next save or sync"
                   (error-message-string err))))))

(defun maybe-enable-orgist ()
  "Enable `orgist-mode' if the current Org file is orgist-managed.
Checks for the `TODOIST-PROJECT' file property.  When re-enabling
after a revert, skips the initial sync to avoid redundant API calls."
  (when (and (not orgist-mode)
             (org-entry-get (point-min) "TODOIST-PROJECT" t))
    (let ((orgist--skip-initial-sync t))
      (orgist-mode 1))))

(add-hook 'org-mode-hook #'maybe-enable-orgist)

;;; Interactive functions
;;;###autoload
(defun orgist ()
  "Synchronize projects, sections and tasks from Todoist."
  (interactive)
  (setq orgist-sync-mutex (current-time))
  (setq orgist--pull-counts nil)
  (let ((orgist--log-buffer '()))
    (unwind-protect
        (progn
          (orgist-log-start-session)
          ;; Detect and (dry-run) write back local changes before pulling.
          ;; In 'ask mode, write-back shows a confirmation buffer and
          ;; defers the pull to a continuation callback.
          (if (and orgist-enable-write-back
                   (eq (orgist-write-back #'orgist-pull) 'pending))
              ;; Confirmation buffer is waiting for user — pull will
              ;; happen via the continuation passed above.
              (orgist-log 'debug "Awaiting write-back confirmation before sync")
            (orgist-pull)))
      (orgist--flush-log-buffer))))

(defcustom orgist-async-chunk-size 30
  "Threshold for subprocess sync in interactive Emacs.
When the number of items exceeds this value, sync processing is
offloaded to an `emacs --batch' subprocess so the UI stays
responsive.  Batch mode always processes synchronously."
  :group 'orgist
  :type 'integer)

(defun orgist-pull (&optional retries)
  "Pull latest state from Todoist Sync API.
This is the read half of `orgist' — separated so it can be called
as a continuation after interactive write-back confirmation.
In interactive Emacs, large item sets are processed in a
subprocess (`emacs --batch') so the UI stays responsive.
RETRIES is the current retry count (0 on first call); transient
errors (curl SSL, timeout, HTTP 429/5xx) are retried up to
`orgist-request-retry-max' times with exponential backoff."
  (setq retries (or retries 0))
  (when (zerop retries)
    (orgist-log 'info "Syncing..."))
  (let ((sync-token (if (file-exists-p orgist-sync-token-filename)
                        (with-temp-buffer
                          (insert-file-contents orgist-sync-token-filename)
                          (buffer-string))
                      "*")))
    (request
      "https://api.todoist.com/api/v1/sync"
      :headers `(("Authorization" . ,(format "Bearer %s" orgist-bearer-token)))
      :data `(("sync_token" . ,sync-token)
              ("resource_types" . "[\"projects\", \"sections\", \"items\", \"labels\", \"reminders\", \"collaborators\", \"user\", \"user_plan_limits\"]"))
      :parser 'json-read
      :error (cl-function (lambda (&key (data nil) error-thrown symbol-status
                                  response &allow-other-keys)
                            (let* ((status-code (when response
                                                  (request-response-status-code response)))
                                   (transport (orgist--transport-error-p error-thrown))
                                   (retryable (or transport
                                                  (orgist--retryable-http-p status-code)
                                                  (eql status-code 429)))
                                   (detail (or (alist-get 'error data)
                                               (and (consp error-thrown)
                                                    (cdr error-thrown))
                                               error-thrown)))
                              (if (and retryable (< retries orgist-request-retry-max))
                                  (let ((delay (if (eql status-code 429)
                                                   (or (alist-get 'retry_after data) 5)
                                                 (* 2 (1+ retries)))))
                                    (orgist-log 'warn
                                                "Sync error (%s), retrying in %ds (%d/%d): %S"
                                                (if transport "transport" (format "HTTP %s" status-code))
                                                delay (1+ retries) orgist-request-retry-max detail)
                                    (orgist--flush-log-buffer)
                                    (run-at-time delay nil #'orgist-pull (1+ retries)))
                                (setq orgist-sync-mutex nil)
                                (orgist-log 'warn "Sync API error: HTTP %s %s — %S"
                                            (or status-code "?") (or symbol-status "?")
                                            detail)
                                (orgist--flush-log-buffer)
                                (error "Sync API error: HTTP %s — %S"
                                       (or status-code "?") detail)))))
      :success (cl-function
                (lambda (&key data &allow-other-keys)
                  (condition-case err
                  (let* ((sync-token (alist-get 'sync_token data))
                         (projects (alist-get 'projects data))
                         (sections (alist-get 'sections data))
                         (items (alist-get 'items data))
                         (filter-ids (orgist-resolve-project-filter projects)))
                    ;; Store additional sync resources
                    (orgist-store-user-profile data)
                    (orgist-store-labels data)
                    (orgist-store-reminders data)
                    (orgist-store-collaborators data)
                    (when-let* ((limits (alist-get 'user_plan_limits data)))
                      (setq orgist-plan-limits limits))
                    (orgist-apply-label-faces)
                    (setq orgist--pull-counts
                          (list (length projects) (length sections) (length items)))
                    (orgist-log 'debug "Sync response: %d projects, %d sections, %d items"
                                (length projects) (length sections) (length items))
                    (when filter-ids
                      (setq projects (seq-filter
                                      (lambda (p) (member (alist-get 'id p) filter-ids))
                                      projects))
                      (setq sections (seq-filter
                                      (lambda (s) (member (alist-get 'project_id s) filter-ids))
                                      sections))
                      (setq items (seq-filter
                                   (lambda (i) (member (alist-get 'project_id i) filter-ids))
                                   items))
                      (orgist-log 'debug "Filtered to project(s) %s: %d projects, %d sections, %d items"
                                  orgist-sync-project-filter
                                  (length projects) (length sections) (length items)))
                    (cond
                     ((and orgist-sync-project-filter (not filter-ids))
                      (orgist-log 'warn "Aborting sync: project filter '%s' matched nothing"
                                  orgist-sync-project-filter)
                      (setq orgist--pull-counts '(0 0 0))
                      (orgist--finish-pull))
                     (t
                      (if (and (not noninteractive)
                               (> (length items) orgist-async-chunk-size))
                          ;; Large set in interactive mode: offload to subprocess.
                          ;; Sync token is saved by the subprocess after
                          ;; successful processing (not before).
                          (orgist--subprocess-pull data)
                        ;; Small set or batch mode: synchronous in-process.
                        (let ((inhibit-redisplay t)
                              (orgist--batch-save-pending (make-hash-table :test 'eq))
                              (gc-cons-threshold (* 100 1024 1024))
                              (gc-cons-percentage 0.6))
                          (orgist-update-projects (orgist-sort-hierarchically projects))
                          (orgist-update-elements sections 'section)
                          (orgist-update-elements
                           (orgist-sort-hierarchically items) 'item)
                          (orgist--flush-pending-saves)
                          ;; Save sync token only after successful processing.
                          ;; Don't save when filtering, as we skip other
                          ;; projects' changes and would lose them.
                          (unless orgist-sync-project-filter
                            (orgist-save-sync-token sync-token))
                          (orgist--finish-pull))))))
                    (error
                     (setq orgist-sync-mutex nil)
                     (orgist-log 'warn "Error during sync processing: %s"
                                 (error-message-string err))
                     (orgist--flush-log-buffer)
                     (message "Orgist: sync error — %s"
                              (error-message-string err)))))))))


(defun orgist--subprocess-locale-config ()
  "Return a list of sexps to configure locale and encoding in a subprocess.
Environment variables (LANG, LC_ALL, TZ, etc.) are inherited
automatically by `make-process', but `emacs -Q --batch' does not
run the user's init.el so the Emacs-internal settings derived from
those variables are missing.  This propagates the parent's language
environment, locale coding system, and process coding system.
Splice into the subprocess program with `,@'."
  `(;; Language environment controls default coding systems for
    ;; buffers, files, and new strings (e.g. \"UTF-8\" → utf-8
    ;; defaults).
    (set-language-environment ,current-language-environment)
    ;; Locale coding system — set-locale-environment cannot reliably
    ;; parse POSIX locale strings on Windows, so propagate the
    ;; parent's value directly.
    (setq locale-coding-system ',locale-coding-system)
    ;; Process coding system for subprocess I/O and file operations
    ;; (e.g. utf-8-dos on Windows instead of the default undecided).
    (setq default-process-coding-system
          ',(default-value 'default-process-coding-system))))

(defun orgist--subprocess-org-config ()
  "Return a list of sexps to configure org-mode in a subprocess.
Captures the parent Emacs's org settings at call time for splicing
into a backquoted subprocess program with `,@'."
  `((setq org-element-use-cache nil)
    (setq org-priority-highest
          ,(if (boundp 'org-priority-highest) org-priority-highest 1))
    (setq org-priority-lowest
          ,(if (boundp 'org-priority-lowest) org-priority-lowest 5))
    (setq org-priority-default
          ,(if (boundp 'org-priority-default) org-priority-default 5))
    (setq org-log-into-drawer
          ,(if (boundp 'org-log-into-drawer) org-log-into-drawer nil))
    (setq org-log-done
          ',(if (boundp 'org-log-done) org-log-done nil))
    (setq org-log-note-headings
          ',(if (boundp 'org-log-note-headings) org-log-note-headings nil))
    (setq org-log-states-order-reversed
          ,(if (boundp 'org-log-states-order-reversed)
               org-log-states-order-reversed t))
    (setq org-todo-keywords
          ',(if (boundp 'org-todo-keywords)
                org-todo-keywords
              '((sequence "TODO" "DONE"))))))

(defun orgist--run-subprocess (plist)
  "Spawn an Emacs subprocess with shared boilerplate.
PLIST is a property list with these keys:
  :name          — process name string
  :data-files    — list of temp files to clean up
  :done-file     — path to done-file
  :needs-http    — non-nil to propagate request-backend/curl/gnutls settings
  :extra-settings — list of extra `(setq ...)' sexps
  :open-org-files — non-nil to open .org files and build id-caches
  :job-body      — list of sexps for the subprocess to execute
  :on-success    — function(count) called on completion
  :on-failure    — function(event) called on failure"
  (let* ((name (plist-get plist :name))
         (data-files (plist-get plist :data-files))
         (done-file (plist-get plist :done-file))
         (needs-http (plist-get plist :needs-http))
         (extra-settings (plist-get plist :extra-settings))
         (open-org-files (plist-get plist :open-org-files))
         (job-body (plist-get plist :job-body))
         (on-success (plist-get plist :on-success))
         (on-failure (plist-get plist :on-failure))
         ;; Resolve paths
         (emacs-path (concat invocation-directory invocation-name))
         (orgist-el-dir orgist--directory)
         (confirm-dir (when-let* ((f (symbol-file 'orgist-confirm-show)))
                        (file-name-directory f)))
         (token (if (functionp orgist-bearer-token)
                    (funcall orgist-bearer-token)
                  orgist-bearer-token))
         (dep-paths (seq-filter
                     (lambda (p)
                       (or (string-match-p "/request" p)
                           (string-match-p "/org/" p)
                           (string-match-p "/org$" p)))
                     load-path))
         (all-paths (delete-dups
                     (delq nil
                           (append (list orgist-el-dir confirm-dir)
                                   dep-paths))))
         (program
          ;; print-length/print-level truncate lists with "..." which
          ;; the subprocess would interpret as a void variable reference.
          (let ((print-length nil)
                (print-level nil))
            (format "%S"
                    `(progn
                     ;; Load dependencies — inherit relevant load-path
                     ;; entries from the parent Emacs.
                     ,@(mapcar (lambda (p)
                                 `(add-to-list 'load-path ,p))
                               all-paths)
                     (load ,(expand-file-name "orgist.el" orgist-el-dir)
                           nil nil t)
                     ;; Inherit locale and coding-system settings
                     ;; from parent Emacs.
                     ,@(orgist--subprocess-locale-config)
                     ;; HTTP: inherit request.el backend and TLS settings
                     ,@(when needs-http
                         `((setenv "TZ" ,(getenv "TZ"))
                           (require 'request nil t)
                           (setq request-backend
                                 ',(if (boundp 'request-backend)
                                       request-backend 'url-retrieve))
                           (setq request-curl
                                 ,(if (boundp 'request-curl)
                                      request-curl
                                    (executable-find "curl")))
                           (setq gnutls-trustfiles
                                 ',(if (boundp 'gnutls-trustfiles)
                                       gnutls-trustfiles nil))))
                     ;; Configure orgist
                     (setq orgist-base-dir ,orgist-base-dir)
                     (setq orgist-sync-token-filename ,orgist-sync-token-filename)
                     (setq orgist-snapshot-file ,orgist-snapshot-file)
                     (setq orgist-labels-file ,orgist-labels-file)
                     ;; No log file in subprocess — output goes to
                     ;; stdout/stderr, captured by parent's process filter.
                     (setq orgist-log-file nil)
                     (setq orgist-log-level ',orgist-log-level)
                     (setq orgist-bearer-token ,token)
                     (setq orgist-tag ,orgist-tag)
                     (setq orgist-treat-priority-4-as-none
                           ,orgist-treat-priority-4-as-none)
                     ;; Enable write-back so snapshots are recorded
                     ;; during element updates; the subprocess never
                     ;; calls orgist-write-back itself.
                     (setq orgist-enable-write-back t)
                     (setq orgist-write-back-dry-run t)
                     (setq orgist-project-buffer-cache nil)
                     ;; Load existing snapshots from disk so
                     ;; incremental syncs preserve entries for
                     ;; elements not in this batch (and retain
                     ;; comment-ids, activity-ids, etc.).
                     (orgist-load-snapshots)
                     (setq orgist-sync-mutex nil)
                     (setq revert-without-query '(".*"))
                     ;; Disable file locking — the parent Emacs may hold
                     ;; locks on .org files, and batch mode cannot prompt
                     ;; "steal the lock?", so save-buffer would error.
                     (setq create-lockfiles nil)
                     ;; Suppress "file changed on disk" conflicts — the
                     ;; parent Emacs may save files while the subprocess
                     ;; is running, and batch mode cannot prompt.
                     ;; Override the C-level entry point directly so that
                     ;; even if userlock.el is autoloaded later (which
                     ;; would overwrite ask-user-about-supersession-threat),
                     ;; the wrapper that C code actually calls is already
                     ;; neutralized.
                     (defun userlock--ask-user-about-supersession-threat (_) nil)
                     (defun ask-user-about-supersession-threat (_) nil)
                     ;; Org-mode settings — must match the parent
                     ;; Emacs so headings, logbook entries, and
                     ;; timestamps are created identically.
                     ,@(orgist--subprocess-org-config)
                     ;; Extra settings from caller
                     ,@extra-settings
                     ;; Open org files and build id-caches
                     ,@(when open-org-files
                         `((orgist-load-snapshots t)
                           (dolist (file (directory-files orgist-base-dir t "\\`[^.].*\\.org\\'"))
                             (find-file file)
                             (org-mode)
                             (orgist-build-id-cache))))
                     ;; Job body
                     ,@job-body)))))
    ;; Remove stale done-file
    (when (file-exists-p done-file)
      (delete-file done-file))
    (let ((process
           (make-process
            :name name
            :command (list emacs-path "-Q" "--batch" "--eval" program)
            :noquery t
            :sentinel
            (lambda (_proc event)
              (cond
               ((string-match-p "finished" event)
                (orgist-log 'debug "%s subprocess finished" name)
                (let ((count (when (file-exists-p done-file)
                               (with-temp-buffer
                                 (insert-file-contents done-file)
                                 (string-to-number (string-trim (buffer-string)))))))
                  ;; Clean up temp files
                  (dolist (f data-files)
                    (when (file-exists-p f)
                      (delete-file f)))
                  (when (file-exists-p done-file)
                    (delete-file done-file))
                  ;; Revert all orgist buffers from disk
                  (orgist--revert-buffers-from-disk)
                  ;; Reload snapshots from disk
                  (orgist-load-snapshots t)
                  (orgist--flush-log-buffer)
                  ;; Call on-success
                  (when on-success
                    (funcall on-success count))))
               (t
                (orgist-log 'warn "%s subprocess failed: %s"
                            name (string-trim event))
                ;; Clean up temp files
                (dolist (f data-files)
                  (when (file-exists-p f)
                    (delete-file f)))
                (orgist--flush-log-buffer)
                (if on-failure
                    (funcall on-failure event)
                  (message "Orgist: %s failed — %s"
                           name (string-trim event)))))))))
      ;; Log all subprocess output to file (debug level = file only,
      ;; not *Messages*); relay Orgist [INFO/WARN] lines to minibuffer.
      ;; Wrap in condition-case so errors in the filter don't
      ;; cascade (the subprocess sentinel handles the failure).
      (set-process-filter
       process
       (lambda (_proc output)
         (condition-case err
             (dolist (line (split-string output "\n" t))
               (when (length> line 0)
                 (orgist-log 'debug "%s: %s" name line))
               (cond
                ((string-match "\\`Orgist \\[WARN\\] \\(.*\\)" line)
                 (message "Orgist [WARN] %s" (match-string 1 line)))
                ((string-match "\\`Orgist \\[INFO\\] \\(.*\\)" line)
                 (message "Orgist: %s" (match-string 1 line)))))
           (error
            (message "Orgist: %s filter error: %S" name err))))))))

(defun orgist--subprocess-pull (data)
  "Process sync DATA in a subprocess to avoid blocking the UI.
Writes the JSON response to a temp file, spawns `emacs --batch'
to run the full sync (projects, sections, items), then reverts
buffers and reloads snapshots when the subprocess finishes."
  (let* ((json-file (expand-file-name "subprocess-data.json" orgist-base-dir))
         (done-file (expand-file-name "subprocess-done" orgist-base-dir)))
    ;; Write JSON to temp file
    (with-temp-file json-file
      (let ((json-encoding-pretty-print nil))
        (insert (json-encode data))))
    (orgist-log 'debug "Spawning subprocess for sync (%d items)"
                (length (alist-get 'items data)))
    (orgist-log 'debug "Orgist: syncing %d items in background..."
                (length (alist-get 'items data)))
    (orgist--run-subprocess
     (list
      :name "orgist-sync"
      :data-files (list json-file)
      :done-file done-file
      :needs-http nil
      :extra-settings `((setq orgist-sync-project-filter ,orgist-sync-project-filter)
                         (setq orgist-sync-attachments ,orgist-sync-attachments))
      :open-org-files nil
      :job-body `(;; Read JSON and process
                  (let* ((json-object-type 'alist)
                         (json-array-type 'vector)
                         (json-key-type 'symbol)
                         (data (json-read-file ,json-file))
                         (sync-token (alist-get 'sync_token data))
                         (projects (alist-get 'projects data))
                         (sections (alist-get 'sections data))
                         (items (alist-get 'items data))
                         (filter-ids (orgist-resolve-project-filter projects))
                         (inhibit-redisplay t)
                         (orgist--batch-save-pending
                          (make-hash-table :test 'eq))
                         (gc-cons-threshold (* 100 1024 1024))
                         (gc-cons-percentage 0.6))
                    (when filter-ids
                      (setq projects
                            (seq-filter
                             (lambda (p)
                               (member (alist-get 'id p) filter-ids))
                             projects))
                      (setq sections
                            (seq-filter
                             (lambda (s)
                               (member (alist-get 'project_id s) filter-ids))
                             sections))
                      (setq items
                            (seq-filter
                             (lambda (i)
                               (member (alist-get 'project_id i) filter-ids))
                             items)))
                    ;; Process everything
                    (orgist-update-projects
                     (orgist-sort-hierarchically projects))
                    (orgist-update-elements sections 'section)
                    (orgist-update-elements
                     (orgist-sort-hierarchically items) 'item)
                    (orgist--flush-pending-saves)
                    ;; Save snapshots
                    (orgist-save-snapshots)
                    ;; Save sync token only after successful processing.
                    ;; Don't save when filtering, as we skip other
                    ;; projects' changes and would lose them.
                    (unless orgist-sync-project-filter
                      (orgist-save-sync-token sync-token))
                    ;; Signal completion
                    (with-temp-file ,done-file
                      (insert "done\n"))
                    (message "orgist subprocess: sync complete")))
      :on-success (lambda (_count)
                    (orgist--finish-pull)
                    (message "Orgist: sync complete"))
      :on-failure (lambda (event)
                    (setq orgist-sync-mutex nil)
                    (message "Orgist: sync failed — %s"
                             (string-trim event)))))))

(defun orgist--revert-buffers-from-disk ()
  "Revert all orgist-managed buffers from their files on disk.
Skips buffers with unsaved modifications to avoid destroying
the user's in-progress edits, and buffers whose file is unchanged
on disk — reverting wipes the org-element cache and forces a full
re-parse, which blocks Emacs when run from a process sentinel.
Suppresses confirmation prompts, rebuilds ID caches, and re-folds
drawers."
  (dolist (file (directory-files orgist-base-dir t "\\`[^.].*\\.org\\'"))
    (when-let* ((buf (find-buffer-visiting file)))
      (cond
       ((buffer-modified-p buf)
        (orgist-log 'debug "Skipping revert of %s (unsaved changes)"
                    (file-name-nondirectory file)))
       ((verify-visited-file-modtime buf))
       (t
        (condition-case err
            (with-current-buffer buf
              (let ((revert-without-query '(".*")))
                (revert-buffer t t t))
              (orgist-build-id-cache)
              ;; revert-buffer resets folding state; re-fold all drawers
              ;; so PROPERTIES/LOGBOOK drawers don't appear expanded.
              (org-cycle-hide-drawers 'all))
          (error
           (orgist-log 'warn "Error reverting %s: %s"
                       (file-name-nondirectory file)
                       (error-message-string err)))))))))

(defun orgist--finish-pull ()
  "Finalize a pull: save snapshots, clear mutex, flush logs.
When `orgist-sync-comments' is non-nil, chains a comment/activity
pull in a subprocess."
  (let ((counts (or orgist--pull-counts '(0 0 0))))
    (if (and (zerop (nth 0 counts))
             (zerop (nth 1 counts))
             (zerop (nth 2 counts)))
        (orgist-log 'info "Syncing... done, no changes")
      (orgist-log 'info "Syncing... done: %d projects, %d sections, %d items"
                  (nth 0 counts) (nth 1 counts) (nth 2 counts))))
  ;; Persist snapshots only when the pull actually processed items.
  ;; A no-change pull has nothing new to record; the old habit of
  ;; saving unconditionally re-wrote a 4500-entry file every few
  ;; minutes and bumped the mtime that the pre-stamp modified-file
  ;; check compared against, masking pending local changes within
  ;; seconds of a failed write-back (observed 2026-08-12).
  (when (and orgist-enable-write-back
             orgist--pull-counts
             (not (equal orgist--pull-counts '(0 0 0))))
    (orgist-save-snapshots))
  (setq orgist--last-pull-time (current-time))
  (setq orgist-sync-mutex nil)
  (orgist--flush-log-buffer)
  ;; Ensure orgist-mode is enabled on all managed buffers.
  ;; Buffers opened by the agenda before the token was set, or
  ;; opened by find-file-noselect during sync, may not have the
  ;; mode active because org-mode-hook only fires on first activation.
  (dolist (file (directory-files orgist-base-dir t "\\`[^.].*\\.org\\'"))
    (when-let* ((buf (find-buffer-visiting file)))
      (with-current-buffer buf
        (when (and (not orgist-mode)
                   (derived-mode-p 'org-mode))
          (let ((orgist--skip-initial-sync t))
            (orgist-mode 1))))))
  ;; Chain completed tasks pull first (so comments pull covers them too).
  ;; If only comments enabled, chain comments directly.
  (when (not noninteractive)
    (cond
     (orgist-sync-completed-tasks
      (orgist--subprocess-completed-tasks-pull))
     (orgist-sync-comments
      (orgist--subprocess-comments-pull)))))

(defun orgist-save-sync-token (sync-token)
  "Save Todoist API sync_token to a file."
  (unless (file-directory-p (file-name-directory orgist-sync-token-filename))
    (make-directory (file-name-directory orgist-sync-token-filename) t))
  (with-temp-file orgist-sync-token-filename
    (insert sync-token)))

(defun orgist-reset ()
  "Delete all orgist data, caches, and internal state."
  (interactive)
  (when (y-or-n-p "Delete all orgist data?")
    ;; Kill any running subprocesses
    (dolist (name '("orgist-sync" "orgist-comments" "orgist-completed"))
      (when-let* ((proc (get-process name)))
        (when (process-live-p proc)
          (delete-process proc))))
    ;; Kill every buffer visiting a file inside orgist-base-dir.
    ;; Use `set-buffer-modified-p' to avoid "save?" prompts, and
    ;; clear `buffer-file-name' to prevent Emacs lock-file warnings.
    (let ((dir (expand-file-name orgist-base-dir)))
      (dolist (buf (buffer-list))
        (when-let* ((file (buffer-file-name buf)))
          (when (string-prefix-p dir (expand-file-name file))
            (with-current-buffer buf
              (set-buffer-modified-p nil)
              (setq buffer-file-name nil))
            (kill-buffer buf)))))
    (setq orgist-project-buffer-cache nil)
    (setq orgist-snapshots nil)
    (setq orgist-sync-mutex nil)
    (setq orgist--last-pull-time nil)
    (setq orgist--write-back-stamps nil)
    (setq orgist--write-back-stamps-path nil)
    (setq orgist--pending-stamps nil)
    (setq orgist--auto-pull-deferred nil)
    (orgist--stop-auto-pull)
    (when (file-directory-p orgist-base-dir)
      (condition-case err
          (delete-directory orgist-base-dir t)
        (file-error
         ;; On Windows, reserved names like NUL can't be deleted.
         ;; Delete what we can and warn about the rest.
         (orgist-log 'warn "Could not fully delete %s: %s"
                     orgist-base-dir (error-message-string err)))))
    (orgist)))

;;; Sync resource storage

(defun orgist-store-user-profile (data)
  "Extract and store user profile from sync DATA.
Sets `orgist-user-timezone' and `orgist-user-inbox-project-id'."
  (when-let* ((user (alist-get 'user data)))
    (when-let* ((tz (alist-get 'timezone user)))
      (setq orgist-user-timezone tz)
      (orgist-log 'debug "User timezone: %s" tz))
    (when-let* ((inbox-id (alist-get 'inbox_project_id user)))
      (setq orgist-user-inbox-project-id inbox-id)
      (orgist-log 'debug "Inbox project: %s" inbox-id))))

(defun orgist-store-labels (data)
  "Extract and store label definitions from sync DATA.
Only processes non-empty arrays (incremental sync returns [] for unchanged).
Persists the updated table to `orgist-labels-file' so it survives restarts."
  (when-let* ((labels (alist-get 'labels data)))
    (when (length> labels 0)
      (unless orgist-labels
        (setq orgist-labels (make-hash-table :test 'equal)))
      (seq-doseq (label labels)
        (let ((id (alist-get 'id label))
              (is-deleted (eq (alist-get 'is_deleted label) t)))
          (if is-deleted
              (remhash id orgist-labels)
            (puthash id label orgist-labels))))
      (orgist-log 'debug "Labels: %d updated, %d total"
                  (length labels) (hash-table-count orgist-labels))
      (orgist-save-labels))))

(defun orgist-save-labels ()
  "Persist `orgist-labels' to `orgist-labels-file'."
  (when orgist-labels
    (let ((dir (file-name-directory orgist-labels-file)))
      (unless (file-directory-p dir)
        (make-directory dir t))
      (with-temp-file orgist-labels-file
        (insert ";; orgist labels -- do not edit\n")
        (let ((entries '()))
          (maphash (lambda (id label) (push (cons id label) entries))
                   orgist-labels)
          (prin1 entries (current-buffer))
          (insert "\n")))
      (orgist-log 'debug "Saved %d labels to %s"
                  (hash-table-count orgist-labels) orgist-labels-file))))

(defun orgist-load-labels ()
  "Load `orgist-labels' from `orgist-labels-file' if not already in memory."
  (when (and (not orgist-labels)
             (file-exists-p orgist-labels-file))
    (setq orgist-labels (make-hash-table :test 'equal))
    (with-temp-buffer
      (insert-file-contents orgist-labels-file)
      (goto-char (point-min))
      (forward-line 1)
      (let ((entries (ignore-errors (read (current-buffer)))))
        (dolist (entry entries)
          (puthash (car entry) (cdr entry) orgist-labels))))
    (orgist-log 'debug "Loaded %d labels from %s"
                (hash-table-count orgist-labels) orgist-labels-file)))

(defun orgist--refresh-labels-from-api ()
  "Fetch the complete label list from the Todoist API into `orgist-labels'.
Incremental syncs only return labels that changed since the last
sync token, so the cache can be missing labels that were never
touched while orgist was watching.  Called lazily from write-back
before an unknown label triggers a label_add, so that label_add is
only sent for labels that genuinely don't exist — a code 54
\"already exists\" failure then remains a real error signal.
Returns non-nil on success, nil on request failure."
  (let ((fetched nil))
    (orgist--request-with-retry
      "https://api.todoist.com/api/v1/sync"
      :headers `(("Authorization" . ,(format "Bearer %s" orgist-bearer-token)))
      :data '(("sync_token" . "*")
              ("resource_types" . "[\"labels\"]"))
      :parser 'json-read
      :sync t
      :timeout orgist-write-back-timeout
      :error (cl-function
              (lambda (&key data error-thrown &allow-other-keys)
                (orgist-log 'warn "Label refresh API error: %S (err=%S)"
                            data error-thrown)))
      :success (cl-function
                (lambda (&key data &allow-other-keys)
                  (orgist-store-labels data)
                  (setq fetched t))))
    (when fetched
      (orgist-log 'info "Refreshed label cache: %d label(s)"
                  (if orgist-labels (hash-table-count orgist-labels) 0)))
    fetched))

(defun orgist-store-reminders (data)
  "Extract and store reminders from sync DATA, grouped by item ID.
Only processes non-empty arrays (incremental sync returns [] for unchanged).
Merges incrementally — does NOT clear existing reminders."
  (when-let* ((reminders (alist-get 'reminders data)))
    (when (length> reminders 0)
      (unless orgist-reminders
        (setq orgist-reminders (make-hash-table :test 'equal)))
      (seq-doseq (reminder reminders)
        (let* ((item-id (alist-get 'item_id reminder))
               (reminder-id (alist-get 'id reminder))
               (is-deleted (eq (alist-get 'is_deleted reminder) t))
               (existing (gethash item-id orgist-reminders)))
          (if is-deleted
              ;; Remove this specific reminder from the item's list
              (let ((filtered (seq-remove
                               (lambda (r) (equal (alist-get 'id r) reminder-id))
                               existing)))
                (if filtered
                    (puthash item-id filtered orgist-reminders)
                  (remhash item-id orgist-reminders)))
            ;; Add or update: replace existing reminder with same ID, or append
            (let ((updated (seq-remove
                            (lambda (r) (equal (alist-get 'id r) reminder-id))
                            existing)))
              (puthash item-id (cons reminder updated) orgist-reminders)))))
      (orgist-log 'debug "Reminders: %d updated, %d items have reminders"
                  (length reminders) (hash-table-count orgist-reminders)))))

(defun orgist-store-collaborators (data)
  "Extract and store collaborator profiles from sync DATA.
Only processes non-empty arrays (incremental sync returns [] for unchanged)."
  (when-let* ((collabs (alist-get 'collaborators data)))
    (when (length> collabs 0)
      (unless orgist-collaborators
        (setq orgist-collaborators (make-hash-table :test 'equal)))
      (seq-doseq (collab collabs)
        (let ((uid (alist-get 'id collab)))
          (puthash uid collab orgist-collaborators)))
      (orgist-log 'debug "Collaborators: %d updated, %d total"
                  (length collabs)
                  (hash-table-count orgist-collaborators)))))

(defconst orgist-todoist-color-map
  '(("berry_red" . "#b8256f") ("red" . "#db4035")
    ("orange" . "#ff9933") ("yellow" . "#fad000")
    ("olive_green" . "#afb83b") ("lime_green" . "#7ecc49")
    ("green" . "#299438") ("mint_green" . "#6accbc")
    ("teal" . "#158fad") ("sky_blue" . "#14aaf5")
    ("light_blue" . "#96c3eb") ("blue" . "#4073ff")
    ("grape" . "#884dff") ("violet" . "#af38eb")
    ("lavender" . "#eb96eb") ("magenta" . "#e05194")
    ("salmon" . "#ff8d85") ("charcoal" . "#808080")
    ("grey" . "#b8b8b8") ("taupe" . "#ccac93"))
  "Alist mapping Todoist color names to hex values.")

(defun orgist-apply-label-faces ()
  "Apply org tag faces based on Todoist label colors.
Sets `org-tag-faces' entries for labels that have known colors.
Uses org-safe tag names (hyphens/spaces replaced with underscores)."
  (when orgist-labels
    (let ((faces '()))
      (maphash
       (lambda (_id label)
         (when-let* ((name (alist-get 'name label))
                     (tag (orgist--label-to-tag name))
                     (color (alist-get 'color label))
                     (hex (cdr (assoc color orgist-todoist-color-map))))
           (push (cons tag `(:foreground ,hex)) faces)))
       orgist-labels)
      (when faces
        (setopt org-tag-faces (append faces
                                    (seq-remove
                                     (lambda (f) (assoc (car f) faces))
                                     org-tag-faces)))
        (orgist-log 'debug "Applied %d label face(s)" (length faces))))))

(defun orgist--label-to-tag (label-name)
  "Convert Todoist LABEL-NAME to an org-safe tag.
Org-mode tags only support alphanumeric, underscore, @, #, and %.
Remove hyphens and spaces, capitalizing the following character
\(CamelCase): \"Self-care\" → \"SelfCare\", \"My Label\" → \"MyLabel\"."
  (if (null label-name) label-name
    (replace-regexp-in-string
     "[-  ]\\(.\\)"
     (lambda (match) (upcase (match-string 1 match)))
     label-name)))

(defun orgist--tag-to-label (tag)
  "Convert org TAG back to the original Todoist label name.
Looks up the tag in `orgist-labels' by comparing sanitized names.
Returns the original Todoist label name if found, or TAG as-is
for labels created locally."
  (if (or (null orgist-labels) (null tag))
      tag
    (catch 'found
      (maphash
       (lambda (_id label)
         (when-let* ((name (alist-get 'name label)))
           (when (equal (orgist--label-to-tag name) tag)
             (throw 'found name))))
       orgist-labels)
      tag)))

(defun orgist-label-name-to-id (name)
  "Look up label ID by NAME (or org-tag equivalent).  Returns the label ID or nil.
Compares using `orgist--label-to-tag' so that org-safe tag names
like \"Selfcare\" match Todoist labels like \"Self-care\"."
  (when orgist-labels
    (let ((tag (orgist--label-to-tag name)))
      (catch 'found
        (maphash
         (lambda (id label)
           (when (equal (orgist--label-to-tag (alist-get 'name label)) tag)
             (throw 'found id)))
         orgist-labels)
        nil))))

(defun orgist-resolve-collaborator-name (uid)
  "Resolve collaborator UID to display name.
Returns the full_name if found, otherwise the UID string."
  (or (when-let* ((table orgist-collaborators)
                   (collab (gethash uid table)))
        (alist-get 'full_name collab))
      uid))

;;; Project filtering
(defun orgist-resolve-project-filter (projects)
  "Resolve `orgist-sync-project-filter' to a list of project IDs.
Returns nil if no filter is set.  Includes sub-projects of matching
projects."
  (when orgist-sync-project-filter
    (let* ((matching (seq-filter
                      (lambda (p)
                        (string= (alist-get 'name p) orgist-sync-project-filter))
                      projects))
           (ids (mapcar (lambda (p) (alist-get 'id p)) matching)))
      (if ids
          (progn
            ;; Also include sub-projects whose parent_id matches
            (seq-doseq (p projects)
              (when (member (alist-get 'parent_id p) ids)
                (push (alist-get 'id p) ids)))
            (orgist-log 'debug "Project filter matched IDs: %s" ids)
            ids)
        (orgist-log 'warn "Project filter '%s' matched no projects"
                    orgist-sync-project-filter)
        nil))))

;;; Project
(defun orgist-update-projects (projects)
  "Update org-mode files with projects from Todoist."
  (seq-doseq (project projects)
    (let* ((project-id (alist-get 'id project))
           (is-deleted (eq (alist-get 'is_deleted project) t))
           (project-name (alist-get 'name project))
           (project-buffer (orgist-get-project-buffer project-id))
           (parent-id (alist-get 'parent_id project)))
      (orgist-log 'debug "update-projects: %s (parent=%s deleted=%s)"
                  (orgist--id-label project-id project-name) parent-id is-deleted)
      (cond
       (is-deleted (orgist-delete-project project-id))
       (project-buffer (orgist-update-project project))
       (parent-id (orgist-create-subproject project))
       (t (orgist-create-root-project project))))))

(defun orgist-delete-project (project-id)
  "Delete the org file associated with PROJECT-ID."
  (if-let* ((project-buffer (orgist-get-project-buffer project-id)))
      (let ((file-path (buffer-file-name project-buffer)))
        (kill-buffer project-buffer)
        (setq orgist-project-buffer-cache
              (assoc-delete-all project-id orgist-project-buffer-cache))
        (when file-path (delete-file file-path))
        (orgist-log 'warn "Deleted project file: %s" file-path))
    (orgist-log 'warn "Cannot delete project %s: buffer not found" project-id)))

(defun orgist-create-subproject (project)
  "Create a subproject as a heading in the parent project's file."
  (let* ((project-id (alist-get 'id project))
         (project-name (alist-get 'name project))
         (parent-id (alist-get 'parent_id project))
         (parent-buffer (orgist-get-project-buffer parent-id)))
    (with-current-buffer parent-buffer
      (goto-char (point-min))
      (unless (search-forward (concat "* " project-name) nil t)
        (goto-char (point-max))
        (insert (concat "* " project-name "\n")))
      (org-set-property "ID" project-id)
      (orgist-id-cache-put project-id)
      ;; Add to project-buffer cache so sections/items can find this subproject
      (push (cons project-id parent-buffer) orgist-project-buffer-cache)
      (orgist--save-buffer))))

(defun orgist-create-root-project (project)
  "Create a root project as a standalone org file."
  (let* ((project-id (alist-get 'id project))
         (project-name (alist-get 'name project))
         (org-file-path (orgist-get-project-file-path project-name)))
    ;; Ensure the directory exists
    (unless (file-directory-p orgist-base-dir)
      (make-directory orgist-base-dir t))
    (let ((buf (find-file-noselect org-file-path)))
      (with-current-buffer buf
        (goto-char (point-min))
        (unless (search-forward "#+FILETAGS:" nil t)
          (insert (concat "#+FILETAGS: :" orgist-tag ":\n")))
        (unless (search-forward "#+TITLE:" nil t)
          (insert (concat "#+TITLE: " project-name "\n\n")))
        (org-set-property "ID" project-id)
        (org-set-property "TODOIST-PROJECT" "")
        (orgist-id-cache-put project-id)
        (orgist--save-buffer))
      ;; Add to project-buffer cache
      (push (cons project-id buf) orgist-project-buffer-cache))))

(defun orgist-get-project-file-path (project-name)
  "Get the file path for a project based on its name."
  (expand-file-name
   (concat (replace-regexp-in-string "[/:*?\"<>|]" "_" project-name) ".org")
   orgist-base-dir))

(defun orgist-update-project (project)
  "Update an existing PROJECT with new properties.
  Handles changes to name, parent_id, and other project properties."
  (let* ((project-id (alist-get 'id project))
         (new-name (alist-get 'name project))
         (new-parent-id (alist-get 'parent_id project))
         (project-buffer (orgist-get-project-buffer project-id))
         (current-file-path (buffer-file-name project-buffer))
         (is-root-project (not new-parent-id))
         (was-root-project (orgist-is-root-project-p project-id current-file-path)))
    (orgist-log 'debug "update project %s new-parent-id %s is-root %s was-root %s current-file-path %s" (orgist--id-label project-id new-name) new-parent-id is-root-project was-root-project current-file-path)
    (cond
     ;; Case 1: Root project -> subproject (move to parent file)
     ((and was-root-project (not is-root-project))
      (orgist-convert-root-to-subproject project project-buffer))

     ;; Case 2: Subproject -> root project (extract to new file)
     ((and (not was-root-project) is-root-project)
      (orgist-convert-subproject-to-root project project-buffer))

     ;; Case 3: Subproject staying subproject
     ((and (not was-root-project) (not is-root-project))
      (orgist-update-subproject project project-buffer))

     ;; Case 4: Root project staying root (update in place)
     ((and was-root-project is-root-project)
      (orgist-update-root-project-in-place project project-buffer)))))

(defun orgist-is-root-project-p (project-id file-path)
  "Check if PROJECT-ID is a root project in FILE-PATH.
A root project has its ID as the file-level ID property."
  (when file-path
    (with-current-buffer (find-file-noselect file-path)
      (string= project-id (or (org-entry-get (point-min) "ID" t) "")))))

(defun orgist-convert-root-to-subproject (project project-buffer)
  "Convert a root project to a subproject by moving it to the parent's file."
  (orgist-log 'debug "orgist-convert-root-to-subproject %s"
              (orgist--id-label (alist-get 'id project) (alist-get 'name project)))
  (let* ((project-id (alist-get 'id project))
         (project-name (alist-get 'name project))
         (parent-id (alist-get 'parent_id project))
         (parent-buffer (orgist-get-project-buffer parent-id))
         (current-file-path (buffer-file-name project-buffer)))

    ;; Copy all content from current file to parent file as a heading
    (with-current-buffer project-buffer
      (let ((content (buffer-string)))
        (with-current-buffer parent-buffer
          (goto-char (point-max))
          (insert (concat "\n* " project-name "\n"))
          (org-set-property "ID" project-id)
          ;; Insert original content under this heading (adjust levels)
          (let ((adjusted-content (orgist-adjust-content-for-subproject content)))
            (when adjusted-content
              (insert adjusted-content)))
          (orgist--save-buffer))))

    ;; Remove from cache and delete original file
    (setq orgist-project-buffer-cache
          (assoc-delete-all project-id orgist-project-buffer-cache))
    (kill-buffer project-buffer)
    (delete-file current-file-path)

    ;; Update cache to point to parent buffer
    (push (cons project-id parent-buffer) orgist-project-buffer-cache)))

(defun orgist-convert-subproject-to-root (project project-buffer)
  "Convert a subproject to a root project by extracting it to a new file."
  (let* ((project-id (alist-get 'id project))
         (project-name (alist-get 'name project))
         (new-file-path (orgist-get-project-file-path project-name)))
    (orgist-log 'debug "orgist-convert-subproject-to-root %s %s %s" project-id project-name new-file-path)
    ;; Find the subproject heading in current buffer
    (with-current-buffer project-buffer
      (let ((project-point (orgist-find-element-by-id project-id)))
        (when project-point
          (goto-char project-point)
          ;; Extract the subtree content (org-copy-subtree puts it in kill ring)
          (org-copy-subtree 1 t)
          (let ((subtree-content (current-kill 0)))
            ;; Create new file
            (with-current-buffer (find-file-noselect new-file-path)
              (erase-buffer)
              (insert (concat "#+TITLE: " project-name "\n\n"))
              (org-set-property "ID" project-id)
              (org-set-property "TODOIST-PROJECT" "")
              ;; Insert content (adjust heading levels)
              (let ((adjusted-content (orgist-adjust-content-for-root-project subtree-content)))
                (when adjusted-content
                  (insert adjusted-content)))
              (orgist--save-buffer))
            (orgist--save-buffer))))

      ;; Update cache
      (let ((new-buffer (find-file-noselect new-file-path)))
        (setq orgist-project-buffer-cache
              (cons (cons project-id new-buffer)
                    (assoc-delete-all project-id orgist-project-buffer-cache)))))))

(defun orgist-update-subproject (project project-buffer)
  "Update a subproject heading.  Creates it if missing, moves it if
parent changed, or updates name in place."
  (let* ((project-id (alist-get 'id project))
         (project-name (alist-get 'name project))
         (new-parent-id (alist-get 'parent_id project))
         (new-parent-buffer (orgist-get-project-buffer new-parent-id)))
    (with-current-buffer project-buffer
      (save-excursion
        (let ((project-point (orgist-find-element-by-id project-id)))
          (cond
           ;; Heading doesn't exist yet — create it
           ((not project-point)
            (orgist-log 'debug "Subproject %s not found, creating heading in %s"
                        (orgist--id-label project-id project-name) (buffer-name new-parent-buffer))
            (orgist-create-subproject project))
           ;; Same file — just update the heading name
           ((eq project-buffer new-parent-buffer)
            (orgist-log 'debug "Updating subproject %s name" (orgist--id-label project-id project-name))
            (goto-char project-point)
            (org-edit-headline project-name)
            (orgist--save-buffer))
           ;; Different file — move the subtree
           (t
            (orgist-log 'debug "Moving subproject %s from %s to %s"
                        (orgist--id-label project-id project-name) (buffer-name project-buffer) (buffer-name new-parent-buffer))
            (goto-char project-point)
            (org-copy-subtree 1 t)
            (let ((subtree-content (current-kill 0)))
              (with-current-buffer new-parent-buffer
                (save-excursion
                  (goto-char (point-max))
                  (insert "\n")
                  (insert subtree-content)
                  (orgist--save-buffer)))
              (orgist--save-buffer))
            ;; Update cache
            (setq orgist-project-buffer-cache
                  (cons (cons project-id new-parent-buffer)
                        (assoc-delete-all project-id orgist-project-buffer-cache))))))))))

(defun orgist-update-root-project-in-place (project project-buffer)
  "Update a root project's properties in place."
  (orgist-log 'debug "orgist-update-root-project-in-place %s"
              (orgist--id-label (alist-get 'id project) (alist-get 'name project)))
  (let* ((new-name (alist-get 'name project))
         (current-file-path (buffer-file-name project-buffer))
         (new-file-path (orgist-get-project-file-path new-name)))

    (with-current-buffer project-buffer
      (save-excursion
        ;; Update title
        (goto-char (point-min))
        (if (re-search-forward "^#\\+TITLE: .*$" nil t)
            (replace-match (concat "#+TITLE: " new-name))
          (goto-char (point-min))
          (insert (concat "#+TITLE: " new-name "\n")))
        (orgist--save-buffer)))

    ;; Rename file if name changed
    (unless (string= current-file-path new-file-path)
      (rename-file current-file-path new-file-path)
      (with-current-buffer project-buffer
        (set-visited-file-name new-file-path)
        (orgist--save-buffer)))))

(defun orgist-adjust-content-for-subproject (content)
  "Adjust content from a root project file to be inserted under a
subproject heading.  Removes file-level properties and increases
heading levels."
  (orgist-log 'debug "orgist-adjust-content-for-subproject")
  (when content
    (with-temp-buffer
      (insert content)
      (goto-char (point-min))

      ;; Remove file-level properties (#+TITLE, #+PROPERTY, etc.)
      (while (re-search-forward "^#\\+[A-Z_]+:.*\n" nil t)
        (replace-match ""))

      ;; Remove file-level property drawer if it exists
      (goto-char (point-min))
      (when (re-search-forward "^[ \t]*:PROPERTIES:\n\\(?:[ \t]*:.*:.*\n\\)*[ \t]*:END:\n?" nil t)
        (replace-match ""))

      ;; Increase all heading levels by 1
      (goto-char (point-min))
      (while (re-search-forward "^\\(\\*+\\)" nil t)
        (replace-match (concat "*" (match-string 1))))

      ;; Clean up extra blank lines at the beginning
      (goto-char (point-min))
      (while (looking-at "^\n")
        (delete-char 1))

      (string-trim (buffer-string)))))

(defun orgist-adjust-content-for-root-project (subtree-content)
  "Adjust subtree content to be inserted in a root project file.
  Removes the top-level heading and decreases all heading levels by 1."
  (when subtree-content
    (with-temp-buffer
      (insert subtree-content)
      (goto-char (point-min))

      ;; Remove the first heading line (the project heading itself)
      (when (re-search-forward "^\\*+ .*\n" nil t)
        (replace-match ""))

      ;; Remove the property drawer for this heading
      (when (re-search-forward "^[ \t]*:PROPERTIES:\n\\(?:[ \t]*:.*:.*\n\\)*[ \t]*:END:\n?" nil t)
        (replace-match ""))

      ;; Decrease all remaining heading levels by 1
      (goto-char (point-min))
      (while (re-search-forward "^\\(\\*\\{2,\\}\\)" nil t)
        (replace-match (substring (match-string 1) 1)))

      ;; Clean up extra blank lines
      (goto-char (point-min))
      (while (looking-at "^\n")
        (delete-char 1))

      (let ((result (string-trim (buffer-string))))
        (if (string-empty-p result) nil result)))))

;;; Elements (section or task)
(defun orgist-update-elements (elements element-type)
  "Update org-mode elements with elements from Todoist."
  (let ((total (length elements))
        (count 0))
    (when (> total 0)
      (orgist-log 'debug "Updating %ss with %d elements"
                  (symbol-name element-type) total))
    (seq-doseq (element elements)
      (setq count (1+ count))
      (let* ((element-id (alist-get 'id element))
             (project-id (alist-get 'project_id element))
             (section-id (alist-get 'section_id element))
             (parent-id (alist-get 'parent_id element))
             (child-order (alist-get 'child_order element))
             (section-order (alist-get 'section_order element))
             (element-name (or (alist-get 'content element) (alist-get 'name element)))
             (label (orgist--id-label element-id element-name))
             (is-deleted (eq (alist-get 'is_deleted element) t))
             ;; Deleted elements come back with their project_id stripped
             ;; to a placeholder, so the owning buffer can't be resolved;
             ;; don't bother looking it up.
             (project-buffer (unless is-deleted
                               (orgist-get-project-buffer project-id))))
        (when (= (% count 100) 0)
          (orgist-log 'debug "Progress: [%d/%d] %ss %s" count total (symbol-name element-type) label))
        (when (= count total)
          (orgist-log 'debug "Progress: [%d/%d] %ss %s" count total (symbol-name element-type) label))
        (orgist-log 'debug "[%d/%d] %s %s (parent=%s section=%s project=%s order=%s)"
                    count total (symbol-name element-type) label
                    parent-id section-id project-id
                    (or section-order child-order))
        (cond
         (is-deleted
          (orgist-delete-element element-id label))
         ((not project-buffer)
          (orgist-log 'warn "No buffer found for project %s, skipping element %s"
                      project-id label))
         (t
          ;; Cross-project move: when the element already lives in
          ;; another project buffer, transplant its subtree into the
          ;; target buffer so logbook history, CLOSED stamps and local
          ;; children survive instead of being recreated from Todoist
          ;; data.  Copies still present once the element exists in the
          ;; target buffer are stale duplicates and are deleted.
          ;; Use `find-buffer-visiting' — these buffers were already
          ;; opened during project creation; avoids stale-file prompts.
          (let ((transplanted nil)
                (in-target (with-current-buffer project-buffer
                             (orgist-find-element-by-id element-id))))
            (dolist (file (directory-files orgist-base-dir t "\\`[^.].*\\.org\\'"))
              (when-let* ((buf (find-buffer-visiting file)))
                (unless (eq buf project-buffer)
                  (with-current-buffer buf
                    (save-excursion
                      (when-let* ((old-point (orgist-find-element-by-id element-id)))
                        (goto-char old-point)
                        (if in-target
                            (progn
                              (orgist-log 'debug "Removing duplicate %s %s from %s"
                                          (symbol-name element-type) label (buffer-name buf))
                              (orgist-delete-subtree))
                          (orgist-log 'debug "Transplanting moved %s %s from %s to %s"
                                      (symbol-name element-type) label
                                      (buffer-name buf) (buffer-name project-buffer))
                          (orgist--transplant-subtree project-buffer)
                          (setq in-target t
                                transplanted t))
                        (orgist-build-id-cache)
                        (orgist--save-buffer)))))))
          ;; Process element
          (with-current-buffer project-buffer
            (save-excursion
              (let* ((todoist-parent-id (or parent-id section-id project-id))
                     (parent-point (orgist-find-element-by-id todoist-parent-id)))
                (if (not parent-point)
                    (orgist-log 'warn "Parent not found for %s %s (parent: %s) in %s, skipping"
                                (symbol-name element-type) label
                                todoist-parent-id
                                (buffer-name project-buffer))
                  ;; 1. Find or create element
                  (let ((is-new nil)
                        (was-reparented nil)
                        (order (or section-order child-order)))
                    (if-let* ((existing-point (orgist-find-element-by-id element-id)))
                        (progn
                          (orgist-log 'debug "Found existing element %s at point %d"
                                      label existing-point)
                          (goto-char existing-point)
                          ;; 2. Reparent if parent changed
                          (setq was-reparented
                                (orgist-reparent-if-needed element-id todoist-parent-id)))
                      ;; Insert new element
                      (orgist-log 'debug "Creating new element %s at parent point %d"
                                  label parent-point)
                      (goto-char parent-point)
                      (orgist-insert-element element)
                      (setq is-new t))
                    ;; 3. Update content/properties
                    (orgist-update-element element is-new)
                    ;; 4. Reposition among siblings if new, reparented,
                    ;;    transplanted from another file, or order changed
                    (let* ((old-order (org-entry-get (point) "TODOIST-ORDER"))
                           (needs-position (or is-new
                                              was-reparented
                                              transplanted
                                              (not (equal old-order
                                                         (number-to-string order))))))
                      (org-set-property "TODOIST-ORDER"
                                        (number-to-string order))
                      (when needs-position
                        (orgist-log 'debug "Repositioning %s (old-order=%s new-order=%s)"
                                    label old-order order)
                        (orgist-position-element-by-order order element-type)
                        ;; org-move-subtree can eat blank lines between a
                        ;; parent's body text and its first child heading,
                        ;; and the trailing blank line of the moved subtree
                        ;; itself.  Re-normalize both.
                        (orgist--normalize-body-spacing)
                        (save-excursion
                          (when (org-up-heading-safe)
                            (orgist--normalize-body-spacing)))
                        ;; Rebuild cache: org-move-subtree invalidates markers
                        (orgist-rebuild-subtree-cache)))
                    (orgist--save-buffer)))))))))))))

(defun orgist-reparent-if-needed (element-id todoist-parent-id)
  "Move element at point to TODOIST-PARENT-ID if its org parent differs.
Point must be on the element's heading.  After reparenting, point is
on the moved heading at its new location and the ID cache is rebuilt.
Returns non-nil if reparenting occurred."
  (org-back-to-heading t)
  (let* ((current-org-parent-id
          (save-excursion
            (if (org-up-heading-safe)
                (org-entry-get (point) "ID")
              ;; Top-level heading — parent is the file-level ID
              (org-entry-get (point-min) "ID"))))
         (target-level (orgist-calculate-heading-level todoist-parent-id)))
    (when (not (equal current-org-parent-id todoist-parent-id))
      (orgist-log 'debug "Reparenting %s: %s -> %s"
                  (orgist--id-label element-id (org-get-heading t t t t))
                  current-org-parent-id todoist-parent-id)
      ;; Cut the subtree
      (org-cut-subtree)
      ;; Go to new parent and paste as child
      (let ((parent-point (orgist-find-element-by-id todoist-parent-id)))
        (goto-char parent-point)
        (goto-char (orgist--subtree-end))
        ;; Paste the subtree
        (org-paste-subtree (or target-level 1))
        ;; Point is now on the pasted heading
        (org-back-to-heading t))
      ;; Rebuild cache since we moved text around
      (orgist-rebuild-subtree-cache)
      t)))

(defun orgist-insert-element (element)
  "Create a new org-mode heading for a Todoist ELEMENT at point.
Only creates the heading structure and sets the ID property.
Content is filled in by `orgist-update-element'."
  (let* ((project-id (alist-get 'project_id element))
         (section-id (alist-get 'section_id element))
         (parent-id (alist-get 'parent_id element))
         (id (alist-get 'id element))
         (level (orgist-calculate-heading-level (or parent-id section-id project-id))))
    (orgist-log 'debug "Inserting element %s under %s (target level %s)"
                (orgist--id-label id (or (alist-get 'content element) (alist-get 'name element)))
                (or parent-id section-id project-id) level)
    (org-insert-heading-respect-content)
    (insert "placeholder")
    (let ((current-level (org-current-level)))
      (when (and level (> level current-level))
        (dotimes (_ (- level current-level))
          (org-demote-subtree))))
    (org-set-property "ID" id)
    (orgist-id-cache-put id)))

(defun orgist-update-element (element &optional skip-clear-body)
  "Update org-mode node at point with Todoist ELEMENT.
Point should be at or near the heading.  This function navigates to
the heading before making changes.  When SKIP-CLEAR-BODY is non-nil,
skip the `orgist-clear-body' call (useful for newly created elements
that have no body to clear)."
  (org-back-to-heading-or-point-min t)
  (let* (;; `org-todo'/`org-schedule'/`org-deadline' loop over all headlines
         ;; in the active region; a user selection at pull time must never
         ;; smear this element's state and dates across other headings.
         (org-loop-over-headlines-in-active-region nil)
         ;; Todoist state is authoritative on pull: org blocking rules
         ;; (e.g. `org-enforce-todo-dependencies') must not silently veto
         ;; DONE on a parent whose Todoist subtasks are still open.
         (org-blocker-hook nil)
         (content (alist-get 'content element))
         (name (alist-get 'name element))
         (description (alist-get 'description element))
         (labels (alist-get 'labels element))
         (due (alist-get 'due element))
         (deadline (alist-get 'deadline element))
         (duration (alist-get 'duration element))
         (id (alist-get 'id element))
         (project-id (alist-get 'project_id element))
         (section-id (alist-get 'section_id element))
         (parent-id (alist-get 'parent_id element))
         (added-at (alist-get 'added_at element))
         (level (or (orgist-calculate-heading-level (or parent-id section-id project-id))
                    (org-current-level)
                    1))
         ;; Pre-update state, for synthesizing reopen/repeat log entries.
         (old-state (org-get-todo-state))
         (old-scheduled (org-entry-get (point) "SCHEDULED"))
         ;; A done-type keyword the user chose (e.g. CANCELED) is kept:
         ;; Todoist only knows checked/unchecked, so any done keyword
         ;; already satisfies checked=t.
         (new-state (when content
                      (if (eq (alist-get 'checked element) t)
                          (if (member old-state org-done-keywords) old-state "DONE")
                        "TODO"))))
    (orgist-log 'debug "Updating element %s at point %d"
                (orgist--id-label id (or content name)) (point))
    ;; Set heading text (convert markdown formatting to org)
    (org-edit-headline (orgist-convert-content (or content name "")))
    ;; Set TODO state for tasks, mark sections.
    ;; The keyword is applied silently: sync replays *past* remote
    ;; events, so a note logged by `org-todo' would stamp the sync time
    ;; instead of the event time, and re-applying an unchanged state
    ;; (e.g. a catch-up sync after weeks offline) would mint a bogus
    ;; DONE->DONE entry.  State log entries are instead synthesized
    ;; below from Todoist's own timestamps, which also makes replayed
    ;; syncs idempotent: identical entries dedupe, new ones only appear
    ;; when the state or the recurrence date actually changed.
    (if content
        (progn
          ;; Skip no-op transitions: re-entering a done state would
          ;; trigger org-auto-repeat on repeatered tasks.
          (unless (equal old-state new-state)
            (let ((org-inhibit-logging t)
                  (org-log-done nil)
                  (org-log-repeat nil))
              (org-todo new-state)))
          (when-let* ((priority (alist-get 'priority element))
                      (org-priority (orgist-todoist-priority-to-org priority)))
            (org-priority org-priority)))
      ;; Section
      (org-set-property "SECTION" ""))
    (let* ((is-archived (and name (eq (alist-get 'is_archived element) t)))
           (label-tags (when labels
                         (mapcar #'orgist--label-to-tag (append labels nil))))
           (existing-tags (cdr (orgist-extract-heading-and-tags)))
           (tags (append (when (member "ATTACH" existing-tags) '("ATTACH"))
                         (when is-archived '("ARCHIVE"))
                         label-tags)))
      (when (or content              ; task: always sync — labels may have been removed in Todoist
                tags label-tags is-archived
                ;; Clear tags when previously archived section is unarchived
                (and name (member "ARCHIVE" existing-tags)))
        (org-set-tags tags)
        (orgist-log 'debug "Setting tags: %s" tags)))
    ;; Set ASSIGNEE property from responsible_uid
    (when-let* ((responsible-uid (alist-get 'responsible_uid element)))
      (let ((assignee-name (orgist-resolve-collaborator-name responsible-uid)))
        (org-entry-put (point) "ASSIGNEE" assignee-name)))
    (when due
      (let ((org-timestamp (orgist-parse-todoist-date-with-duration due duration))
            (due-string (alist-get 'string due)))
        (orgist-log 'debug "Setting schedule: %s" org-timestamp)
        (when org-timestamp
          (org-schedule nil org-timestamp))
        ;; Preserve Todoist's original recurrence string (e.g. "every mon
        ;; at 6:30am") so write-back can send it back verbatim instead of
        ;; reverse-engineering from the org repeater.
        (if due-string
            (org-entry-put (point) "TODOIST_DUE_STRING" due-string)
          (org-entry-delete (point) "TODOIST_DUE_STRING"))))
    (when deadline
      (let ((org-timestamp (orgist-parse-todoist-date-with-duration deadline nil)))
        (orgist-log 'debug "Setting deadline: %s" org-timestamp)
        (when org-timestamp
          (org-deadline nil org-timestamp))))
    ;; Clear old body text (between meta-data/drawers and first child heading)
    ;; Must happen before inserting log entries or description.
    (unless skip-clear-body
      (orgist-clear-body))
    ;; Apply reminders to SCHEDULED/DEADLINE timestamps
    (when-let* ((reminders (and orgist-reminders id
                                (gethash id orgist-reminders))))
      (orgist-apply-reminders reminders))
    ;; Clean up any duplicate logbook entries from previous syncs.
    (orgist-deduplicate-logbook)
    ;; Only tasks get logbook entries instead of CREATED property.
    ;; Skip if logbook already exists (avoids duplicating on re-sync).
    (when (and added-at (not (orgist-has-logbook-p)))
      (let ((created-timestamp (orgist-parse-todoist-timestamp added-at)))
        (orgist-log 'debug "Setting created: %s (task=%s)" created-timestamp (not (null content)))
        (if content
            (orgist-insert-log-entry "TODO" "" created-timestamp)
          (org-set-property "CREATED" created-timestamp))))
    (when-let* ((completed-at (alist-get 'completed_at element))
                (completed-timestamp (orgist-parse-todoist-timestamp completed-at)))
      (unless (orgist-has-logbook-p "DONE")
        (orgist-log 'debug "Setting completed: %s" completed-timestamp)
        (orgist-insert-log-entry "DONE" "TODO" completed-timestamp))
      ;; Set CLOSED planning keyword from Todoist's server-side timestamp.
      ;; Only when absent: preserves the local time if the user closed the
      ;; task in orgist before the sync ran.
      (unless (org-element-property :closed (org-element-at-point))
        (org-back-to-heading t)
        (org-add-planning-info 'closed completed-timestamp)))
    ;; Synthesized entries for state changes org-todo applied silently
    ;; above.  Completions with a completed_at are covered by the branch
    ;; just before; the payload carries no timestamp for the remaining
    ;; cases, so their entries carry the sync time.
    ;; - Remote completion without a completed_at timestamp.
    (when (and new-state
               (member new-state org-done-keywords)
               (not (member old-state org-done-keywords))
               (not (alist-get 'completed_at element))
               (not (orgist-has-logbook-p "DONE")))
      (orgist-insert-log-entry new-state (or old-state "TODO")
                               (format-time-string
                                (org-time-stamp-format 'long 'inactive))))
    ;; - Remote reopen (DONE -> TODO).
    (when (and new-state old-state
               (member old-state org-done-keywords)
               (not (member new-state org-done-keywords)))
      (orgist-insert-log-entry new-state old-state
                               (format-time-string
                                (org-time-stamp-format 'long 'inactive))))
    ;; - Recurring occurrence completed remotely: Todoist advances the
    ;;   due date while the task stays TODO on both sides.  Record the
    ;;   repeat as TODO -> TODO, matching the entries org itself logs
    ;;   for locally-completed repeaters.
    (when (and new-state due
               (eq (alist-get 'is_recurring due) t)
               (equal old-state new-state)
               (not (member new-state org-done-keywords))
               old-scheduled
               (not (equal old-scheduled (org-entry-get (point) "SCHEDULED"))))
      (orgist-insert-log-entry new-state old-state
                               (format-time-string
                                (org-time-stamp-format 'long 'inactive))))
    ;; Active tasks: clear any stale CLOSED left over from a previous
    ;; completion cycle (e.g. recurring task that just advanced).
    ;; A nil TIME makes `org-add-planning-info' prompt for a date;
    ;; removal goes through the REMOVE-LIST argument instead.
    (when (and (not (eq (alist-get 'checked element) t))
               (not (alist-get 'completed_at element))
               (org-element-property :closed (org-element-at-point)))
      (org-back-to-heading t)
      (org-add-planning-info nil nil 'closed))
    ;; Insert description AFTER logbook entries.  `org-end-of-meta-data'
    ;; with argument t skips past property drawers, planning, clocks,
    ;; and bare logbook lines.  Blank-line spacing is fixed by
    ;; `orgist--normalize-body-spacing' below.
    (when (and description (not (string-empty-p description)))
      (orgist-log 'debug "Setting description (%d chars)" (length description))
      (save-excursion
        (org-end-of-meta-data t)
        (let ((converted-description (orgist-convert-description description level)))
          (insert converted-description "\n"))))
    ;; Normalize body spacing: ensure exactly one blank line between
    ;; body content (logbook, description) and the next heading or
    ;; end of subtree.  Never more than one blank line; never a
    ;; description stuck to adjacent content.
    (orgist--normalize-body-spacing)
    ;; Snapshot this element's Todoist state for write-back diffing.
    (when orgist-enable-write-back
      (orgist-snapshot-element element))))

(defun orgist--subtree-end ()
  "Return the end of the subtree at point, validating the element cache.
Point must be on a heading (or before the first heading, in which
case `org-end-of-subtree' covers the whole file).  In Org 9.7,
`org-end-of-subtree' returns the org-element cache's cached
boundary; a stale cached boundary (seen 2026-08-12: an async
cache-sync left an element's :end partially unshifted) points
inside the entry instead of at the next heading.  Downstream that
makes bounded searches signal \"Invalid search bound\", and worse,
feeds wrong bounds to `delete-region' in subtree surgery.

A correct end (TO-HEADING non-nil) is either end-of-buffer or the
start of a heading line strictly after point, so that invariant is
checked here in O(1).  On violation the element cache is reset —
it rebuilds lazily — and the boundary recomputed from the fresh
parse, turning silent corruption into a logged self-repair."
  (let ((end (save-excursion (org-end-of-subtree t t) (point))))
    (if (or (= (point) (point-max))     ; degenerate: empty tail
            (and (> end (point))
                 (or (= end (point-max))
                     (save-excursion
                       (goto-char end)
                       (and (bolp) (looking-at-p org-outline-regexp))))))
        end
      (orgist-log 'warn
                  "Stale org-element cache in %s: subtree at %d reports end %d (not a heading boundary); resetting cache"
                  (buffer-name) (point) end)
      (org-element-cache-reset)
      (save-excursion (org-end-of-subtree t t) (point)))))

(defun orgist--normalize-body-spacing ()
  "Ensure consistent blank-line spacing in the current heading's body.
Guarantees (see README.org § Body Spacing):
- Exactly one blank line separates body content (logbook, description)
  from the next heading or end of subtree.
- Exactly one blank line between logbook entries and description text.
- No runs of more than one consecutive blank line anywhere.
- Tasks/sections with no body content have no trailing blank line.
Point must be on the heading."
  (save-excursion
    (org-back-to-heading-or-point-min t)
    ;; Use `org-end-of-meta-data' WITHOUT t so we stop right after
    ;; property drawer and planning lines — the logbook and description
    ;; are part of the body we need to normalize.
    (org-end-of-meta-data)
    (let ((body-start (point))
          (body-end (save-excursion
                      (let ((subtree-end (orgist--subtree-end)))
                        (if (re-search-forward org-outline-regexp-bol
                                               subtree-end t)
                            (line-beginning-position)
                          subtree-end)))))
      ;; Step 1: Collapse runs of 3+ consecutive newlines to 2.
      (when (< body-start body-end)
        (goto-char body-start)
        (while (re-search-forward "\n\\(\n\\)\\(\n+\\)" body-end t)
          (let ((len (- (match-end 2) (match-beginning 2))))
            (replace-match "" nil nil nil 2)
            (setq body-end (- body-end len)))))
      ;; Step 2: Ensure blank line between logbook entries and
      ;; description text.  Logbook lines match "^- State ".
      ;; Handle both orderings: logbook-then-desc and desc-then-logbook.
      (goto-char body-start)
      (while (re-search-forward
              "^\\(- State .+\\)\n\\([^- \n*]\\)" body-end t)
        (goto-char (match-end 1))
        (insert "\n")
        (setq body-end (1+ body-end)))
      (goto-char body-start)
      (while (re-search-forward
              "^\\([^- \n*:].*\\)\n\\(- State \\)" body-end t)
        (goto-char (match-beginning 2))
        (insert "\n")
        (setq body-end (1+ body-end)))
      ;; Step 2b: Fix spacing at start of body.
      ;; - Remove blank lines between :END:/planning and logbook content
      ;;   (bare "- State ..." lines or :LOGBOOK: drawer).
      ;; - Ensure one blank line before description (non-logbook) content.
      (goto-char body-start)
      (when (< body-start body-end)
        (let ((first-nonblank
               (save-excursion
                 (skip-chars-forward "\n" body-end)
                 (point))))
          (cond
           ;; Body starts with logbook (bare entry or LOGBOOK drawer):
           ;; remove any blank lines before it.
           ((and (< first-nonblank body-end)
                 (save-excursion
                   (goto-char first-nonblank)
                   (looking-at "[ \t]*\\(?:- \\|:LOGBOOK:\\)")))
            (when (> first-nonblank body-start)
              (delete-region body-start first-nonblank)
              (setq body-end (- body-end (- first-nonblank body-start)))))
           ;; Body starts with non-logbook content: ensure one blank line.
           ((and (< first-nonblank body-end)
                 (not (looking-at "\n")))
            (insert "\n")
            (setq body-end (1+ body-end))))))
      ;; Step 2c: Ensure one blank line between a LOGBOOK drawer and the
      ;; description that follows it.  Step 2 only knows bare "- State"
      ;; entries; with `org-log-into-drawer' the entries sit in a drawer
      ;; and the description was inserted right after its :END: line.
      (goto-char body-start)
      (when (and (< body-start body-end)
                 (looking-at "[ \t]*:LOGBOOK:[ \t]*$")
                 (re-search-forward "^[ \t]*:END:[ \t]*\n" body-end t)
                 (< (point) body-end)
                 (not (looking-at "[ \t]*$"))
                 (not (looking-at org-outline-regexp-bol)))
        (insert "\n")
        (setq body-end (1+ body-end)))
      ;; Step 3: Fix trailing boundary.
      (let ((has-body (and (< body-start body-end)
                           (not (string-blank-p
                                 (buffer-substring-no-properties
                                  body-start body-end))))))
        (if has-body
            ;; Ensure exactly one blank line before the next heading.
            (progn
              (goto-char body-end)
              (skip-chars-backward "\n" body-start)
              (forward-char 1)            ; keep one \n
              (unless (= (point) body-end)
                (delete-region (point) body-end)
                (setq body-end (point)))
              (insert "\n"))              ; add the blank line
          ;; No body content — remove any trailing whitespace.
          (when (< body-start body-end)
            (delete-region body-start body-end)))))))

(defun orgist-clear-body ()
  "Delete body text of the current heading.
Preserves only child subtrees that have an ID property (i.e. Todoist
elements).  Deletes all other content: plain text, description
headings from pandoc, and any non-ID child subtrees."
  (save-excursion
    (org-back-to-heading-or-point-min t)
    (org-end-of-meta-data t)
    (let ((pos (point))
          (subtree-end (orgist--subtree-end)))
      ;; Walk through the region, deleting gaps between ID subtrees.
      (while (< pos subtree-end)
        (goto-char pos)
        (if (not (re-search-forward org-outline-regexp-bol subtree-end t))
            ;; No more headings — delete trailing body text.
            (progn
              (when (< pos subtree-end)
                (delete-region pos subtree-end)
                (setq subtree-end pos))
              (setq pos subtree-end))
          ;; Found a heading — check if it has an ID.
          (goto-char (line-beginning-position))
          (if (org-entry-get (point) "ID")
              ;; ID heading: delete gap before it, skip past its subtree.
              (progn
                (when (< pos (point))
                  (let ((gap (- (point) pos)))
                    (delete-region pos (point))
                    (setq subtree-end (- subtree-end gap))))
                ;; Skip past this ID subtree (it's preserved).
                (setq pos (orgist--subtree-end)))
            ;; Non-ID heading: delete its entire subtree.
            (let* ((heading-start (point))
                   (heading-end (orgist--subtree-end))
                   ;; But first, delete the gap before it too.
                   (del-start (min pos heading-start))
                   (del-len (- heading-end del-start)))
              (delete-region del-start heading-end)
              (setq subtree-end (- subtree-end del-len))
              (setq pos del-start))))))))

(defun orgist-has-logbook-p (&optional state)
  "Check if the current heading has a state log entry.
When STATE is non-nil, check for a specific state (e.g. \"DONE\").
Searches only the heading's own body (up to the first child heading
or end of subtree), not into child subtrees.
Handles both default org format (State \"TODO\") and custom
log-note-headings formats (to \"TODO\"), including entries
inside :LOGBOOK: drawers."
  (save-excursion
    (org-back-to-heading-or-point-min t)
    (let* ((subtree-end (orgist--subtree-end))
           ;; Start search right after the heading line so we cover
           ;; :LOGBOOK: drawers (org-end-of-meta-data skips past them).
           (start (save-excursion (forward-line 1) (point)))
           (end (save-excursion
                  (org-end-of-meta-data t)
                  (if (re-search-forward org-outline-regexp-bol subtree-end t)
                      (line-beginning-position)
                    subtree-end)))
           (quoted-state (regexp-quote (or state "TODO"))))
      (goto-char start)
      (re-search-forward
       (format "\\(?:State\\|to\\) \"%s\"" quoted-state)
       end t))))

(defun orgist-deduplicate-logbook ()
  "Remove duplicate log entries from the current heading.
Keeps the first occurrence of each entry and removes subsequent
exact duplicates.  Works with both bare log entries and entries
inside :LOGBOOK: drawers.  Entries are compared including their
continuation lines, so two Notes that share a timestamp but carry
different text are both kept, while exact copies (e.g. a comment
re-inserted by a pull after snapshot loss) are removed.
Returns the number of removed duplicate Note entries."
  (save-excursion
    (org-back-to-heading-or-point-min t)
    (let* ((subtree-end (orgist--subtree-end))
           (start (save-excursion (forward-line 1) (point)))
           (end (save-excursion
                  (org-end-of-meta-data t)
                  (if (re-search-forward org-outline-regexp-bol subtree-end t)
                      (line-beginning-position)
                    subtree-end)))
           ;; When using a :LOGBOOK: drawer, restrict to its contents.
           (drawer-end (save-excursion
                         (goto-char start)
                         (when (re-search-forward "^[ \t]*:LOGBOOK:" end t)
                           (when (re-search-forward "^[ \t]*:END:" end t)
                             (line-beginning-position)))))
           ;; Marker so deletions below keep the bound valid.
           (end (copy-marker (or drawer-end end)))
           (seen (make-hash-table :test 'equal))
           (removed 0)
           (removed-notes 0))
      (goto-char start)
      ;; Collect multi-line entries: each starts with "- " and includes
      ;; all subsequent continuation lines (indented, not a new item).
      (while (re-search-forward "^[ \t]*- " end t)
        (let ((entry-start (line-beginning-position)))
          (forward-line 1)
          (while (and (< (point) end)
                      (not (looking-at "^[ \t]*- "))
                      (not (looking-at "^[ \t]*:END:"))
                      (looking-at "^[ \t]+[^ \t\n-]\\|^[ \t]+$"))
            (forward-line 1))
          (let* ((entry (buffer-substring-no-properties entry-start (point)))
                 (key (string-trim entry)))
            (if (gethash key seen)
                (progn
                  (delete-region entry-start (point))
                  (setq removed (1+ removed))
                  (when (string-match-p "\\(?:Closing \\)?Note \\\\\\\\$"
                                        (car (split-string entry "\n")))
                    (setq removed-notes (1+ removed-notes))))
              (puthash key t seen)))))
      (set-marker end nil)
      (when (> removed 0)
        (orgist-log 'info "Removed %d duplicate logbook entries (%d notes)"
                    removed removed-notes))
      removed-notes)))

(defun orgist--extract-logbook-timestamp (entry)
  "Extract a timestamp string from logbook ENTRY for sorting.
Looks for [YYYY-MM-DD ...] inactive timestamps in the entry text.
Returns the matched timestamp string, or nil if none found."
  (when (string-match "\\[\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}[^]]*\\)\\]" entry)
    (match-string 1 entry)))

(defun orgist-sort-logbook ()
  "Sort logbook entries by timestamp under the current heading.
Respects `org-log-states-order-reversed': when non-nil (default),
newest entries come first; when nil, oldest first.
Handles multi-line entries (Note entries with body text).
Only rewrites the logbook if the order actually changed."
  (save-excursion
    (org-back-to-heading-or-point-min t)
    (let* ((subtree-end (orgist--subtree-end))
           (start (save-excursion (forward-line 1) (point)))
           (end (save-excursion
                  (org-end-of-meta-data t)
                  (if (re-search-forward org-outline-regexp-bol subtree-end t)
                      (line-beginning-position)
                    subtree-end)))
           ;; When using a :LOGBOOK: drawer, restrict to its contents.
           (drawer-end (save-excursion
                         (goto-char start)
                         (when (re-search-forward "^[ \t]*:LOGBOOK:" end t)
                           (when (re-search-forward "^[ \t]*:END:" end t)
                             (line-beginning-position)))))
           (end (or drawer-end end))
           entries region-start region-end)
      ;; Collect multi-line entries: each starts with "- " and includes
      ;; all subsequent continuation lines (indented, not a new list item).
      (goto-char start)
      (while (re-search-forward "^[ \t]*- " end t)
        (let ((entry-start (line-beginning-position)))
          (unless region-start (setq region-start entry-start))
          (forward-line 1)
          ;; Consume continuation lines (indented, not a new list item).
          (while (and (< (point) end)
                      (not (looking-at "^[ \t]*- "))
                      (not (looking-at "^[ \t]*:END:"))
                      (looking-at "^[ \t]+[^ \t\n-]\\|^[ \t]+$"))
            (forward-line 1))
          (setq region-end (point))
          (push (buffer-substring entry-start region-end) entries)))
      (setq entries (nreverse entries))
      (when (length> entries 1)
        (let* ((sorted (sort (copy-sequence entries)
                             (lambda (a b)
                               (let ((ta (orgist--extract-logbook-timestamp a))
                                     (tb (orgist--extract-logbook-timestamp b)))
                                 (cond
                                  ((and ta tb)
                                   (if org-log-states-order-reversed
                                       (string> ta tb)
                                     (string< ta tb)))
                                  (ta (not org-log-states-order-reversed))
                                  (tb org-log-states-order-reversed)
                                  (t nil)))))))
          (unless (equal entries sorted)
            (when (and region-start region-end)
              (delete-region region-start region-end)
              (goto-char region-start)
              (dolist (entry sorted)
                (insert entry))
              (orgist-log 'info "Re-sorted %d logbook entries"
                          (length sorted)))))))))

(defun orgist--safe-log-beginning ()
  "Return the logbook insertion position, guarding against org-element bugs.
`org-log-beginning' can signal `args-out-of-range' when the
heading is near the start of the buffer and org-element-at-point
tries to access position 0.  Fall back to `org-end-of-meta-data'
which is simpler and avoids the element parser."
  (condition-case nil
      (org-log-beginning t)
    (args-out-of-range
     (org-end-of-meta-data)
     (point))))

(defun orgist--log-insertion-point ()
  "Return the position where a new logbook entry should be inserted.
When `org-log-states-order-reversed' is non-nil (default), insert
at the top of the logbook (same as `org-log-beginning').
When nil, insert at the end of existing logbook entries."
  (if org-log-states-order-reversed
      (orgist--safe-log-beginning)
    (save-excursion
      (let ((log-begin (orgist--safe-log-beginning)))
        (goto-char log-begin)
        (let ((bound (save-excursion (org-end-of-meta-data t) (point))))
          ;; Walk forward past all list items (- State, - Note, etc.)
          ;; and their continuation lines (indented text).
          (while (and (< (point) bound)
                      (looking-at "^[ \t]*-\\|^[ \t]+[^ \t\n]"))
            (forward-line 1))
          (point))))))

(defun orgist-insert-log-entry (state old-state timestamp)
  "Insert a log entry for STATE change at TIMESTAMP.
OLD-STATE is the previous state (can be empty string).  TIMESTAMP
should be a time string that can be parsed by
`org-time-string-to-time'."
  (let ((log-beginning (orgist--log-insertion-point))
        (note-template (cdr (assq 'state org-log-note-headings)))
        (effective-time (org-time-string-to-time timestamp)))
    (save-excursion
      (goto-char log-beginning)
      ;; Insert the log entry
      (let ((itemp (org-in-item-p)))
        (if itemp
            (indent-line-to
             (let ((struct (save-excursion
                             (goto-char itemp) (org-list-struct))))
               (org-list-get-ind (org-list-get-top-point struct) struct)))
          (org-indent-line)))
      (insert-and-inherit
       (org-list-bullet-string "-")
       (org-replace-escapes
        note-template
        (list (cons "%s" (format "\"%s\"" state))
              (cons "%S" (if (string-empty-p old-state)
                             ""
                           (format "\"%s\"" old-state)))
              (cons "%t" (format-time-string
                          (org-time-stamp-format 'long 'inactive)
                          effective-time))))
       "\n"))))

;;; Reminders

(defun orgist-apply-reminders (reminders)
  "Apply REMINDERS to the current heading's timestamps.
REMINDERS is a list of reminder alists from the Todoist API.
- Relative reminders: add warning days to SCHEDULED/DEADLINE.
- Absolute reminders: insert active timestamps in the body.
- Location reminders: store as REMINDER-LOC property."
  (dolist (reminder reminders)
    (let ((type (alist-get 'type reminder)))
      (cond
       ;; Relative reminder: add warning days to DEADLINE
       ((string= type "relative")
        (let* ((minute-offset (alist-get 'minute_offset reminder))
               (warning-days (max 1 (/ minute-offset 1440))))
          (when-let* ((deadline-str (org-entry-get (point) "DEADLINE")))
            ;; Only add warning if not already present
            (unless (string-match-p "-[0-9]+[dwm]>" deadline-str)
              (let ((new-deadline (replace-regexp-in-string
                                   ">\\'"
                                   (format " -%dd>" warning-days)
                                   deadline-str)))
                (org-deadline nil new-deadline))))))
       ;; Absolute reminder: insert active timestamp in body
       ((string= type "absolute")
        (when-let* ((due-obj (alist-get 'due reminder)))
          (let ((org-ts (orgist-parse-todoist-date-with-duration due-obj nil)))
            (when org-ts
              ;; Only insert if not already present in body
              (save-excursion
                (org-end-of-meta-data t)
                (let ((body-end (save-excursion
                                  (if (re-search-forward org-outline-regexp-bol nil t)
                                      (line-beginning-position)
                                    (point-max)))))
                  (unless (search-forward org-ts body-end t)
                    (org-end-of-meta-data t)
                    (insert org-ts "\n"))))))))
       ;; Location reminder: store as property
       ((string= type "location")
        (let ((name (alist-get 'name reminder))
              (lat (alist-get 'loc_lat reminder))
              (lon (alist-get 'loc_long reminder)))
          (when name
            (org-entry-put (point) "REMINDER-LOC"
                           (if (and lat lon)
                               (format "%s (%.4f, %.4f)" name lat lon)
                             name)))))))))

;;; Helper functions
(defun orgist-sort-hierarchically (elements)
  "Sort hierarchical elements so parents come before children.
  Uses breadth-first traversal to ensure proper ordering."
  (let ((id-to-element (make-hash-table :test 'equal))
        (children-map (make-hash-table :test 'equal))
        (roots '())
        (orphan-parent-ids '())
        (result '()))
    ;; Build lookup maps
    (seq-doseq (element elements)
      (let ((id (alist-get 'id element))
            (parent-id (alist-get 'parent_id element)))
        ;; Map ID to element
        (puthash id element id-to-element)
        ;; Map parent to children
        (when parent-id
          (let ((siblings (gethash parent-id children-map '())))
            (puthash parent-id (cons element siblings) children-map)))
        ;; Collect roots (elements without parents)
        (unless parent-id
          (push element roots))))
    ;; Collect orphan parent IDs (parents that don't exist as elements)
    (seq-doseq (element elements)
      (let ((parent-id (alist-get 'parent_id element)))
        (when (and parent-id
                   (not (gethash parent-id id-to-element))
                   (not (member parent-id orphan-parent-ids)))
          (push parent-id orphan-parent-ids))))
    ;; Breadth-first traversal from each root
    (dolist (root (reverse roots)) ; Preserve original order of roots
      (let ((queue (list root)))
        (while queue
          (let ((current (pop queue)))
            (push current result)
            ;; Add children to queue (in reverse order to maintain order)
            (let ((children (gethash (alist-get 'id current) children-map '())))
              (setq queue (append queue (reverse children))))))))
    ;; Process orphaned children (children of non-existent parents)
    (dolist (orphan-parent-id (reverse orphan-parent-ids))
      (let ((orphaned-children (gethash orphan-parent-id children-map '())))
        (dolist (orphan (reverse orphaned-children))
          (let ((queue (list orphan)))
            (while queue
              (let ((current (pop queue)))
                (push current result)
                ;; Add children to queue (in reverse order to maintain order)
                (let ((children (gethash (alist-get 'id current) children-map '())))
                  (setq queue (append queue (reverse children))))))))))
    (reverse result)))

(defun orgist-build-id-cache ()
  "Build the buffer-local ID cache by scanning all property drawers.
Markers use insertion-type t so they advance when text is inserted
at their position, preventing stale references after body edits."
  (setq orgist-id-cache (make-hash-table :test 'equal))
  (save-excursion
    (goto-char (point-min))
    ;; Check file-level property drawer
    (when-let* ((file-id (org-entry-get (point-min) "ID")))
      (puthash file-id (copy-marker (point-min) t) orgist-id-cache))
    ;; Scan all headings
    (while (re-search-forward org-property-start-re nil t)
      (when-let* ((id (org-entry-get (point) "ID")))
        (save-excursion
          (org-back-to-heading-or-point-min t)
          (puthash id (copy-marker (point) t) orgist-id-cache))))))

(defun orgist-rebuild-subtree-cache ()
  "Update ID cache for just the subtree at point.
Cheaper than a full `orgist-build-id-cache' when only one
subtree has moved."
  (when orgist-id-cache
    (save-excursion
      (org-back-to-heading-or-point-min t)
      (let ((subtree-end (orgist--subtree-end)))
        (when-let* ((id (org-entry-get (point) "ID")))
          (puthash id (copy-marker (point) t) orgist-id-cache))
        (while (and (re-search-forward org-property-start-re subtree-end t)
                    (<= (point) subtree-end))
          (when-let* ((id (org-entry-get (point) "ID")))
            (save-excursion
              (org-back-to-heading-or-point-min t)
              (puthash id (copy-marker (point) t) orgist-id-cache))))))))

(defun orgist-id-cache-put (element-id)
  "Add current heading's position to the ID cache for ELEMENT-ID."
  (unless orgist-id-cache
    (setq orgist-id-cache (make-hash-table :test 'equal)))
  (save-excursion
    (org-back-to-heading-or-point-min t)
    (puthash element-id (copy-marker (point) t) orgist-id-cache)))

(defun orgist--scan-for-element-id (element-id)
  "Find the heading carrying ELEMENT-ID by scanning the buffer.
Returns the heading position (or `point-min' for the file-level
drawer), or nil when no drawer sets :ID: to ELEMENT-ID.  Linear in
the buffer size — only for the rare cache-repair path."
  (save-excursion
    (goto-char (point-min))
    (let ((re (concat "^[ \t]*:ID:[ \t]+" (regexp-quote element-id) "[ \t]*$"))
          (case-fold-search t)
          (found nil))
      (while (and (not found) (re-search-forward re nil t))
        (let ((pos (save-excursion
                     (org-back-to-heading-or-point-min t)
                     (point))))
          (when (equal (org-entry-get pos "ID") element-id)
            (setq found pos))))
      found)))

(defun orgist-find-element-by-id (element-id)
  "Find an existing element with the given ID in the current buffer.
Uses a buffer-local hash table cache for O(1) lookups.
Returns the point of the element, or nil if not found.
Validates that the heading at the cached position still carries
the expected ID property.  A stale marker (the heading was moved by
a cut/paste or subtree surgery that collapsed its marker onto a
neighbour) is repaired by rescanning the buffer, because the
element is known to live here: returning nil instead makes the
pull path insert a second copy of the heading (seen 2026-09-12).
Only a stale entry triggers the scan — an ID absent from the cache
is simply not in this buffer, and the presence check calls this for
every snapshot ID in every file."
  (unless orgist-id-cache
    (orgist-build-id-cache))
  (when-let* ((marker (gethash element-id orgist-id-cache)))
    (let ((cached (when-let* ((pos (marker-position marker)))
                    (save-excursion
                      (goto-char pos)
                      (org-back-to-heading-or-point-min t)
                      (when (equal (org-entry-get (point) "ID") element-id)
                        (point))))))
      (or cached
          ;; Stale cache entry — rescan before concluding it is gone.
          (let ((pos (orgist--scan-for-element-id element-id)))
            (if pos
                (progn
                  (orgist-log 'debug "Stale id-cache marker for %s repaired by rescan (%s -> %d)"
                              element-id (marker-position marker) pos)
                  (puthash element-id (copy-marker pos t) orgist-id-cache)
                  pos)
              (remhash element-id orgist-id-cache)
              nil))))))

(defun orgist--move-subtree (direction)
  "Move the subtree at point one sibling in DIRECTION (`up' or `down').
`org-move-subtree-up'/`down' take both subtree boundaries from the
org-element cache; a stale cached end (see `orgist--subtree-end')
makes them cut the wrong region, which splits or duplicates
headings.  Validate the cache for this subtree and for the sibling
it will jump over before moving."
  (orgist--subtree-end)
  (save-excursion
    (when (if (eq direction 'down)
              (org-get-next-sibling)
            (org-get-previous-sibling))
      (orgist--subtree-end)))
  (if (eq direction 'down)
      (org-move-subtree-down)
    (org-move-subtree-up)))

(defun orgist-position-element-by-order (child-order element-type)
  "Position the current subtree according to its TODOIST-ORDER.
Moves the subtree so that siblings are sorted by their TODOIST-ORDER
property.  Items and sections are ordered independently — sections
come after all items."
  (org-back-to-heading)
  (let ((heading (org-get-heading t t t t))
        (start-pos (point))
        ;; Suppress the org-move-subtree advice that renumbers every
        ;; sibling's TODOIST-ORDER.  Inbound sync only moves the one
        ;; element whose Todoist order changed; the other siblings'
        ;; orders are still authoritative and must not be rewritten,
        ;; or the next save will emit spurious item_reorder commands
        ;; for the entire parent.
        (orgist--inhibit-sibling-order-update t))
    (orgist-log 'debug "Positioning %s '%s' at point %d (order=%s)"
                (symbol-name element-type) heading start-pos child-order)
    (if (eq element-type 'section)
        ;; Sections: move to bottom among siblings, then sort by order
        ;; among other sections.
        (progn
          (while (save-excursion (org-get-next-sibling))
            (orgist--move-subtree 'down))
          (while (save-excursion
                   (and (org-get-previous-sibling)
                        (org-entry-get (point) "SECTION")
                        (let ((prev-order (string-to-number
                                           (or (org-entry-get (point) "TODOIST-ORDER") "0"))))
                          (> prev-order child-order))))
            (orgist--move-subtree 'up)))
      ;; Items: move all the way up to be the first sibling (items
      ;; always come before sections), then move down past items with
      ;; lower TODOIST-ORDER.  Stop before sections.
      (while (save-excursion (org-get-previous-sibling))
        (orgist--move-subtree 'up))
      ;; Move down past items whose TODOIST-ORDER is lower.
      (while (save-excursion
               (and (org-get-next-sibling)
                    (not (org-entry-get (point) "SECTION"))
                    (let ((next-order (string-to-number
                                       (or (org-entry-get (point) "TODOIST-ORDER") "0"))))
                      (< next-order child-order))))
        (orgist--move-subtree 'down)))
    (when (/= start-pos (point))
      (orgist-log 'debug "Positioned '%s': %d -> %d" heading start-pos (point)))))

(defun orgist-get-project-buffer (project-id)
  "Get the buffer for a Todoist project ID.
First checks cache, then searches through org files in `orgist-base-dir`.
Caches opened buffers for subsequent calls."
  (let ((cached-buffer (alist-get project-id orgist-project-buffer-cache nil nil #'string=)))
    (if (buffer-live-p cached-buffer)
        cached-buffer
      ;; Remove dead buffer from cache and search for the project
      (orgist-log 'debug "Cache miss - %s" project-id)
      (setq orgist-project-buffer-cache
            (assoc-delete-all project-id orgist-project-buffer-cache))
      (when-let* ((project-buffer (catch 'found
                                    (dolist (file (directory-files orgist-base-dir t "\\`[^.].*\\.org\\'"))
                                      (when-let* ((buffer (or (find-buffer-visiting file)
                                                              (find-file-noselect file))))
                                        (with-current-buffer buffer
                                          (when (orgist-find-element-by-id project-id)
                                            (throw 'found buffer)))))
                                    nil)))
        (orgist-log 'debug "Cache - adding %s" project-buffer)
        (push (cons project-id project-buffer) orgist-project-buffer-cache)
        project-buffer))))

(defun orgist-calculate-heading-level (parent-id)
  "Calculate heading level: parent level + 1.
Returns nil if PARENT-ID is not found in the current buffer."
  (save-excursion
    (when-let* ((pos (orgist-find-element-by-id parent-id)))
      (goto-char pos)
      (+ (or (org-current-level) 0) 1))))

(defun orgist--convert-timezone (date-string _from-tz)
  "Convert DATE-STRING from UTC to the Emacs-local timezone.
DATE-STRING is ISO 8601 (YYYY-MM-DDTHH:MM:SS or YYYY-MM-DDTHH:MM:SSZ).
_FROM-TZ is the Todoist timezone hint (currently unused — Todoist
sends UTC for dates with explicit timezones, and `date-to-time'
converts to the local TZ set via $TZ or the system clock).
Returns (DATE-PART . TIME-PART) or nil on failure."
  ;; Validate format up-front: `date-to-time' on unparseable input does
  ;; not signal an error in all Emacs versions — it silently returns
  ;; epoch — so the `condition-case' below isn't sufficient on its own.
  (when (and date-string
             (string-match-p
              "\\`[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}T[0-9]\\{2\\}:[0-9]\\{2\\}:[0-9]\\{2\\}"
              date-string))
    (condition-case nil
        (let* ((cleaned (replace-regexp-in-string "\\.[0-9]+Z\\'" "Z" date-string))
               (cleaned (replace-regexp-in-string "Z\\'" "+0000" cleaned))
               (parsed (date-to-time cleaned)))
          (cons (format-time-string "%Y-%m-%d" parsed)
                (format-time-string "%H:%M" parsed)))
      (error nil))))

(defun orgist-parse-todoist-date-with-duration (date-info duration-info)
  "Parse Todoist date information with duration and convert to org
timestamp with time range. DATE-INFO is an alist containing date,
string, timezone, and is_recurring fields.  DURATION-INFO is an
alist with `amount' and `unit' keys.  Returns an org timestamp
string like '<2025-06-23 Mon 13:00-14:00>' or falls back to
regular date."
  (when (and date-info (alist-get 'date date-info))
    (let* ((date (alist-get 'date date-info))
           (is-recurring (alist-get 'is_recurring date-info))
           (string (alist-get 'string date-info))
           (timezone (alist-get 'timezone date-info))
           (has-time (string-match "[0-9-]+T\\([0-9]\\{2\\}:[0-9]\\{2\\}\\):[0-9:]\\{2\\}" date))
           (start-time (when has-time (match-string 1 date)))
           ;; When the date has an explicit timezone (e.g. "America/New_York")
           ;; or ends in Z (UTC), convert to local time.
           (tz-converted (when (and has-time
                                    (or timezone
                                        (string-suffix-p "Z" date)))
                           (orgist--convert-timezone date timezone)))
           (raw-date-part (if tz-converted
                                (car tz-converted)
                              (if has-time
                                  (substring date 0 (string-match "T" date))
                                date)))
           ;; Normalize to zero-padded YYYY-MM-DD (API may send e.g. 2026-2-3)
           (effective-date-part
            (if (string-match "\\`\\([0-9]\\{4\\}\\)-\\([0-9]+\\)-\\([0-9]+\\)\\'" raw-date-part)
                (format "%04d-%02d-%02d"
                        (string-to-number (match-string 1 raw-date-part))
                        (string-to-number (match-string 2 raw-date-part))
                        (string-to-number (match-string 3 raw-date-part)))
              raw-date-part))
           (effective-start-time (if tz-converted
                                     (cdr tz-converted)
                                   start-time))
           (end-time (when (and effective-start-time duration-info)
                       (orgist-calculate-end-time effective-start-time duration-info))))
      (condition-case err
          (let* ((day-name (orgist--date-string-day-name effective-date-part))
                 (base-timestamp (cond
                                  ((and effective-start-time end-time)
                                   (format "<%s %s %s-%s" effective-date-part day-name
                                           effective-start-time end-time))
                                  (effective-start-time
                                   (format "<%s %s %s" effective-date-part day-name
                                           effective-start-time))
                                  (t
                                   (format "<%s %s" effective-date-part day-name))))
                 (recurrence (when (and (eq is-recurring t) string)
                               (orgist-parse-recurrence-string string))))
            (concat base-timestamp
                    (if recurrence (concat " " recurrence) "")
                    ">"))
        (error
         (orgist-log 'warn "Error parsing Todoist date %s: %s"
                     date (error-message-string err))
         nil)))))

(defun orgist--date-string-day-name (date-string)
  "Return abbreviated day name for DATE-STRING (YYYY-MM-DD).
Uses `calendar-day-of-week' for timezone-independent calculation."
  (let* ((parts (split-string date-string "-"))
         (year (string-to-number (nth 0 parts)))
         (month (string-to-number (nth 1 parts)))
         (day (string-to-number (nth 2 parts)))
         (dow (calendar-day-of-week (list month day year))))
    (aref calendar-day-abbrev-array dow)))

(defun orgist-calculate-end-time (start-time duration-info)
  "Calculate end time given start time and duration.  START-TIME is
in HH:MM format, DURATION-INFO has `amount' and `unit' keys.
Returns end time in HH:MM format."
  (when (and start-time duration-info)
    (let* ((amount (alist-get 'amount duration-info))
           (unit (alist-get 'unit duration-info))
           (duration-minutes (cond
                              ((string= unit "minute") amount)
                              ((string= unit "hour") (* amount 60))
                              ((string= unit "day") (* amount 60 24))
                              (t (* amount 60)))) ; Default to hours
           (time-parts (split-string start-time ":"))
           (start-hour (string-to-number (car time-parts)))
           (start-minute (string-to-number (cadr time-parts)))
           (start-total-minutes (+ (* start-hour 60) start-minute))
           (end-total-minutes (+ start-total-minutes duration-minutes))
           (end-hour (/ end-total-minutes 60))
           (end-minute (% end-total-minutes 60)))
      (orgist-log 'debug "end-time %02d:%02d" end-hour end-minute)
      (format "%02d:%02d" end-hour end-minute))))

(defun orgist-parse-recurrence-string (recurrence-string)
  "Parse Todoist recurrence string and convert to org repeat format.
Handles complex recurrence patterns including dates, times, and frequencies."
  (when recurrence-string
    (let* ((string (downcase (string-trim recurrence-string)))
           ;; Todoist due strings may carry a date prefix before the
           ;; recurrence keyword, e.g. "Feb 18 2026 every week".
           ;; Locate "every!" or "every" and extract from there.
           (every-pos (or (string-match "\\bevery!" string)
                          (string-match "\\bevery\\b" string)))
           ;; Check if this is completion-based ('every!')
           (completion-based (and every-pos
                                  (string-match "\\bevery!" string every-pos)))
           ;; Extract the period part after 'every' or 'every!'
           (period-string (when every-pos
                            (string-trim
                             (substring string
                                        (+ every-pos (if completion-based 6 5))))))
           ;; Strip time component (time is taken from the date field, not
           ;; the recurrence string)
           (date-string (when period-string
                          (orgist-strip-time-component period-string)))
           ;; Parse the date specification
           (period-info (when date-string
                          (orgist-parse-date-specification date-string)))
           (interval (plist-get period-info :interval))
           (unit (plist-get period-info :unit))
           (weekdays (plist-get period-info :weekdays))
           (month-day (plist-get period-info :month-day))
           (month (plist-get period-info :month))
           ;; Determine repeater prefix
           (repeater-prefix (cond
                             (completion-based ".+")
                             ((or weekdays month-day month) "++")
                             (t "+"))))
      (when (and interval unit)
        (format "%s%d%s" repeater-prefix interval unit)))))

(defun orgist-strip-time-component (string)
  "Strip time components from recurrence STRING.
Removes \"at HH:MM(am/pm)\", standalone time patterns, and
\"starting at ...\" clauses.  Returns the cleaned date-only string."
  (thread-last string
    (replace-regexp-in-string
     "\\bat [0-9]+\\(?::[0-9]+\\)?[ \t]*\\(?:am\\|pm\\)?" "")
    (replace-regexp-in-string
     "\\b[0-9]+\\(?::[0-9]+\\)?[ \t]*\\(am\\|pm\\)\\b" "")
    (replace-regexp-in-string
     "\\bstarting at [0-9]+\\(?::[0-9]+\\)?[ \t]*\\(?:am\\|pm\\)?" "")
    (replace-regexp-in-string "\\s-+" " ")
    string-trim))

(defun orgist-month-name-to-number (month-name)
  "Convert MONTH-NAME (e.g. \"jan\", \"february\") to a month number (1-12).
Returns nil if unrecognized."
  (let ((months '(("jan" . 1) ("feb" . 2) ("mar" . 3) ("apr" . 4)
                  ("may" . 5) ("jun" . 6) ("jul" . 7) ("aug" . 8)
                  ("sep" . 9) ("oct" . 10) ("nov" . 11) ("dec" . 12))))
    (cdr (assoc (substring (downcase month-name) 0 3) months))))

(defun orgist-weekday-p (string)
  "Return non-nil if STRING is a weekday name or abbreviation."
  (member (substring (downcase string) 0 (min 3 (length string)))
          '("mon" "tue" "wed" "thu" "fri" "sat" "sun")))

(defun orgist-parse-weekdays (string)
  "Parse a comma-separated list of weekday names from STRING.
Returns a list of 3-letter abbreviations."
  (let ((parts (split-string string "[, ]+" t))
        (result '()))
    (dolist (part parts (nreverse result))
      (let ((abbrev (substring (downcase part) 0 (min 3 (length part)))))
        (when (member abbrev '("mon" "tue" "wed" "thu" "fri" "sat" "sun"))
          (push abbrev result))))))

(defun orgist-parse-date-specification (date-string)
  "Parse date specification part of recurrence string.
Returns property list with keys: :interval, :unit, :weekdays, :month-day, :month"
  (let ((string (string-trim date-string))
        (result (list :interval 1 :unit "d"))) ; Default values

    (cond
     ;; "every other" patterns
     ((string-match "^other \\(day\\|month\\|week\\)$" string)
      (let ((unit-str (match-string 1 string)))
        (plist-put result :interval 2)
        (plist-put result :unit (cond ((string= unit-str "day") "d")
                                      ((string= unit-str "week") "w")
                                      (t "m")))))

     ;; "every other <weekday>" — biweekly on a specific day
     ((string-match "^other " string)
      (let ((rest (substring string 6)))
        (when (orgist-weekday-p rest)
          (plist-put result :interval 2)
          (plist-put result :unit "w")
          (plist-put result :weekdays (list (substring (downcase rest) 0 3))))))

     ;; Special named periods
     ((string-match "^\\(weekday\\|workday\\)s?$" string)
      (plist-put result :unit "d")
      (plist-put result :weekdays '("mon" "tue" "wed" "thu" "fri")))

     ((string-match "^\\(week\\|weekly\\)$" string)
      (plist-put result :unit "w"))

     ((string-match "^weekends?$" string)
      (plist-put result :unit "w")
      (plist-put result :weekdays '("sat")))

     ((string-match "^\\(month\\|monthly\\)$" string)
      (plist-put result :unit "m"))

     ((string-match "^\\(year\\|yearly\\)$" string)
      (plist-put result :unit "y"))

     ((string-match "^hours?$" string)
      (plist-put result :unit "h"))

     ;; Numeric date patterns (MM/DD format)
     ((string-match "^\\([0-9]\\{1,2\\}\\)/\\([0-9]\\{1,2\\}\\)$" string)
      ;; "6/29", "12/31"
      (let ((month (string-to-number (match-string 1 string)))
            (day (string-to-number (match-string 2 string))))
        (plist-put result :unit "y")
        (plist-put result :month month)
        (plist-put result :month-day day)))

     ;; Specific date patterns
     ((string-match "^\\([a-z]+\\) \\([0-9]+\\)\\(?:st\\|nd\\|rd\\|th\\)?$" string)
      ;; "june 29", "jan 27th"
      (let ((month-name (match-string 1 string))
            (day (string-to-number (match-string 2 string))))
        (plist-put result :unit "y")
        (plist-put result :month (orgist-month-name-to-number month-name))
        (plist-put result :month-day day)))

     ;; Monthly day patterns
     ((string-match "^\\([0-9]+\\)\\(?:st\\|nd\\|rd\\|th\\)?$" string)
      ;; "27th", "3rd"
      (plist-put result :unit "m")
      (plist-put result :month-day (string-to-number (match-string 1 string))))

     ((string-match "^\\([0-9]+\\)$" string)
      ;; "27"
      (plist-put result :unit "m")
      (plist-put result :month-day (string-to-number string)))

     ((string-match "^last day$" string)
      (plist-put result :unit "m")
      (plist-put result :month-day -1)) ; Org uses negative for last day

     ;; Ordinal weekday patterns
     ((string-match "^\\([0-9]+\\)\\(?:st\\|nd\\|rd\\|th\\) \\([a-z]+day\\)$" string)
      ;; "3rd friday"
      (let ((weekday (match-string 2 string)))
        (plist-put result :unit "m")
        (plist-put result :weekdays (list (substring weekday 0 3)))))

     ;; Numbered intervals
     ((string-match "^\\([0-9]+\\) \\(days?\\|weeks?\\|months?\\|years?\\|hours?\\)$" string)
      ;; "3 days", "6 weeks"
      (let ((num (string-to-number (match-string 1 string)))
            (unit-str (match-string 2 string)))
        (plist-put result :interval num)
        (plist-put result :unit (string (aref unit-str 0)))))

     ;; Weekday patterns (including comma-separated)
     ((string-match "^\\([a-z,\\s]+day[a-z,\\s]*\\)$" string)
      ;; "mon, fri", "friday"
      (plist-put result :unit "w")
      (plist-put result :weekdays (orgist-parse-weekdays string)))

     ;; Simple weekday patterns
     ((orgist-weekday-p string)
      (plist-put result :unit "w")
      (plist-put result :weekdays (list (substring string 0 3)))))

    result))

(defun orgist--weekday-full-name (abbrev)
  "Expand 3-letter weekday ABBREV (\"mon\") to full lowercase name (\"monday\")."
  (cdr (assoc abbrev '(("mon" . "monday") ("tue" . "tuesday")
                       ("wed" . "wednesday") ("thu" . "thursday")
                       ("fri" . "friday") ("sat" . "saturday")
                       ("sun" . "sunday")))))

(defun orgist--month-number-to-name (n)
  "Return lowercase month name for N (1-12)."
  (nth (1- n) '("january" "february" "march" "april" "may" "june"
                "july" "august" "september" "october" "november" "december")))

(defun orgist--ordinal (n)
  "Return N with English ordinal suffix (e.g. 1 → \"1st\", 22 → \"22nd\")."
  (let* ((mod100 (mod n 100))
         (mod10 (mod n 10))
         (suffix (cond
                  ((<= 11 mod100 13) "th")
                  ((= mod10 1) "st")
                  ((= mod10 2) "nd")
                  ((= mod10 3) "rd")
                  (t "th"))))
    (format "%d%s" n suffix)))

(defun orgist--rebuild-anchored-due-string (stored new-repeater)
  "Reconstruct a Todoist due-string preserving STORED's anchor.
STORED is the previous Todoist due-string (e.g. \"every mon\",
\"every 15\", \"every jan 1\").  NEW-REPEATER is the new org
repeater (e.g. \"++2w\").  Combines STORED's weekday / month-day /
month anchor with NEW-REPEATER's interval, producing a string that
Todoist's NLP accepts.  Returns nil when STORED has no recoverable
anchor — caller should then synthesize a fresh string."
  (when (and stored new-repeater
             (string-match "^\\([.+]+\\)\\([0-9]+\\)\\([dwmyh]\\)" new-repeater))
    (let* ((prefix (match-string 1 new-repeater))
           (interval (string-to-number (match-string 2 new-repeater)))
           (unit-char (match-string 3 new-repeater))
           (every (if (string= prefix ".+") "every!" "every"))
           (period (cond
                    ((string-match "^every! +\\(.*\\)$" stored)
                     (match-string 1 stored))
                    ((string-match "^every +\\(.*\\)$" stored)
                     (match-string 1 stored))))
           (period (and period (downcase (string-trim period))))
           (period (and period (orgist-strip-time-component period)))
           (info (and period (orgist-parse-date-specification period)))
           (weekdays (plist-get info :weekdays))
           (month-day (plist-get info :month-day))
           (month (plist-get info :month))
           (unit-name (cond ((string= unit-char "d") "day")
                            ((string= unit-char "w") "week")
                            ((string= unit-char "m") "month")
                            ((string= unit-char "y") "year")
                            (t unit-char)))
           (unit-plural (if (= interval 1) unit-name (concat unit-name "s")))
           (anchor (cond
                    ((and month month-day)
                     (format "%s %d"
                             (orgist--month-number-to-name month)
                             month-day))
                    (weekdays
                     (mapconcat (lambda (w)
                                  (or (orgist--weekday-full-name w) w))
                                weekdays ", "))
                    (month-day
                     (format "the %s" (orgist--ordinal month-day))))))
      (when anchor
        (if (= interval 1)
            (format "%s %s on %s" every unit-name anchor)
          (format "%s %d %s on %s" every interval unit-plural anchor))))))

(defun orgist-todoist-priority-to-org (todoist-priority)
  "Convert Todoist priority (1-4) to org priority character.
Returns the appropriate priority character based on org-priority settings."
  (cond
   ((= todoist-priority 1)
    (unless orgist-treat-priority-4-as-none org-priority-lowest))
   (t (+ org-priority-highest (- 4 todoist-priority)))))

(defun orgist--pandoc-lua-filter ()
  "Return path to a Lua filter preserving Unicode and local Org links.
Pandoc's org writer downgrades en-dash, em-dash, ellipsis, and
right-quote to ASCII equivalents.  This filter emits the original
Unicode characters as raw org inlines instead.
Markdown readers URL-encode local paths, but Org opens those paths
literally.  Decode local link targets once when importing Markdown;
leave web URLs and other link protocols unchanged.
The file is created once and reused for the session."
  (unless (and orgist--pandoc-lua-filter
               (file-exists-p orgist--pandoc-lua-filter))
    (setq orgist--pandoc-lua-filter
          (make-temp-file "orgist-pandoc-" nil ".lua"))
    (with-temp-file orgist--pandoc-lua-filter
      (insert
       "function Str(el)\n"
       "  local s = el.text\n"
       "  if not (s:find(\"\\u{2013}\") or s:find(\"\\u{2014}\")\n"
       "       or s:find(\"\\u{2026}\") or s:find(\"\\u{2019}\")\n"
       "       or s:find(\"\\u{00AD}\")) then\n"
       "    return nil\n"
       "  end\n"
       "  s = s:gsub(\"\\u{2013}\", \"\\xe2\\x80\\x93\")\n"
       "  s = s:gsub(\"\\u{2014}\", \"\\xe2\\x80\\x94\")\n"
       "  s = s:gsub(\"\\u{2026}\", \"\\xe2\\x80\\xa6\")\n"
       "  s = s:gsub(\"\\u{2019}\", \"\\xe2\\x80\\x99\")\n"
       "  s = s:gsub(\"\\u{00AD}\", \"\\xc2\\xad\")\n"
       "  return pandoc.RawInline(\"org\", s)\n"
       "end\n"
       "function Link(el)\n"
       "  local target = el.target\n"
       "  local scheme = target:match(\"^([%a][%w+.-]*):\")\n"
       "  if scheme == \"attachment\" or scheme == \"file\"\n"
       "     or (not scheme and target:sub(1, 1) ~= \"#\"\n"
       "         and target:sub(1, 2) ~= \"//\") then\n"
       "    el.target = target:gsub(\"%%(%x%x)\", function(hex)\n"
       "      return string.char(tonumber(hex, 16))\n"
       "    end)\n"
       "    return el\n"
       "  end\n"
       "end\n")))
  orgist--pandoc-lua-filter)

(defun orgist-convert-description (description level)
  "Convert markdown description to org-mode format using pandoc if available.
Falls back to original description if pandoc is not available.
Uses `call-process-region' instead of `shell-command-on-region'
so that `exec-path' is respected (the shell may have a different PATH).
Adjusts heading levels to be relative to current org heading."
  (if (executable-find "pandoc")
      (with-temp-buffer
        (insert description)
        (let* ((lua-filter (orgist--pandoc-lua-filter))
               (exit-code
                (call-process-region (point-min) (point-max)
                                     "pandoc" t t nil
                                     "-f" "markdown" "-t" "org"
                                     "--wrap=none"
                                     (concat "--lua-filter=" lua-filter))))
          (if (zerop exit-code)
              (orgist-adjust-heading-levels-and-clean
               (string-trim (buffer-string)) level)
            (orgist-log 'warn "pandoc exited %d, keeping raw description"
                        exit-code)
            description)))
    description))

(defun orgist-convert-content (content)
  "Convert markdown CONTENT (task title) to org-mode format.
Uses pandoc with --wrap=none and collapses the result to a single line.
Skips pandoc when no markdown characters are present."
  (if (or (null content) (string-empty-p content)
          (not (string-match-p "[*`~]\\|\\[.*\\](" content))
          (not (executable-find "pandoc")))
      content
    (let* ((lua-filter (orgist--pandoc-lua-filter))
           (result
            (with-temp-buffer
              (insert content)
              (let ((exit-code
                     (call-process-region (point-min) (point-max)
                                          "pandoc" t t nil
                                          "-f" "markdown" "-t" "org"
                                          "--wrap=none"
                                          (concat "--lua-filter=" lua-filter))))
                (if (zerop exit-code)
                    (string-trim (buffer-string))
                  (orgist-log 'warn "pandoc exited %d for content, keeping raw"
                              exit-code)
                  nil)))))
      (if result
          ;; Collapse to single line: replace newlines with spaces
          (replace-regexp-in-string "\n+" " " result)
        content))))

(defun orgist-convert-content-to-markdown (content)
  "Convert org-mode CONTENT (task title) back to markdown for Todoist.
Uses pandoc with -f org -t gfm --wrap=none and collapses to a single line.
Skips pandoc when no org markup characters are present."
  (if (or (null content) (string-empty-p content)
          (not (string-match-p "[*=~+]\\|\\[\\[" content))
          (not (executable-find "pandoc")))
      content
    (let ((result
           (with-temp-buffer
             (insert content)
             (let ((exit-code
                    (call-process-region (point-min) (point-max)
                                         "pandoc" t t nil
                                         "-f" "org" "-t" "gfm"
                                         "--wrap=none")))
               (if (zerop exit-code)
                   (string-trim (buffer-string))
                 (orgist-log 'warn "pandoc exited %d for content→md, keeping raw"
                             exit-code)
                 nil)))))
      (if result
          (replace-regexp-in-string "\n+" " " result)
        content))))

(defun orgist--org-links-to-markdown (text)
  "Convert org-mode links in TEXT to markdown links.
Handles [[url][desc]] → [desc](url) and [[url]] → url."
  (let ((result text))
    (while (string-match "\\[\\[\\([^][]+\\)\\]\\[\\([^][]+\\)\\]\\]" result)
      (setq result (replace-match "[\\2](\\1)" nil nil result)))
    (while (string-match "\\[\\[\\([^][]+\\)\\]\\]" result)
      (setq result (replace-match "\\1" nil nil result)))
    result))

(defun orgist-convert-description-to-markdown (description)
  "Convert org-mode DESCRIPTION back to markdown for Todoist.
Uses pandoc with -f org -t gfm --wrap=none when available.
Falls back to simple org-link conversion when pandoc is absent."
  (if (or (null description) (string-empty-p description)
          (not (string-match-p "[*=~+/]\\|\\[\\[" description)))
      description
    (if (not (executable-find "pandoc"))
        ;; No pandoc — at least convert org links to markdown links.
        (orgist--org-links-to-markdown description)
      (let ((result
             (with-temp-buffer
               (insert description)
               (let ((exit-code
                      (call-process-region (point-min) (point-max)
                                           "pandoc" t t nil
                                           "-f" "org" "-t" "gfm"
                                           "--wrap=none")))
                 (if (zerop exit-code)
                     (string-trim (buffer-string))
                   (orgist-log 'warn "pandoc exited %d for desc→md, keeping raw"
                               exit-code)
                   nil)))))
        (or result description)))))

(defun orgist-adjust-heading-levels-and-clean (content current-level)
  "Adjust org heading levels in CONTENT to be relative to CURRENT-LEVEL.
Also removes property drawers added by pandoc."
  (orgist-log 'debug "Adjusting headings (current level: %d, content: %d chars)"
              current-level (length content))
  (with-temp-buffer
    (insert content)
    (goto-char (point-min))

    ;; Remove property drawers (including CUSTOM_ID and any other properties)
    (while (re-search-forward "^[ \t]*:PROPERTIES:\n\\(?:[ \t]*:.*:.*\n\\)*[ \t]*:END:\n?" nil t)
      (replace-match ""))

    ;; Adjust heading levels
    (goto-char (point-min))
    (while (re-search-forward "^\\(\\*+\\)\\( \\|$\\)" nil t)
      (let* ((stars (match-string 1))
             (original-level (length stars))
             (new-level (+ current-level original-level))
             (new-stars (make-string new-level ?*)))
        (replace-match (concat new-stars (match-string 2)))))

    ;; Clean up extra blank lines
    (goto-char (point-min))
    (while (re-search-forward "\n\n\n+" nil t)
      (replace-match "\n\n"))

    (string-trim (buffer-string))))

(defun orgist-parse-todoist-timestamp (timestamp-string)
  "Parse Todoist timestamp string and convert to org inactive timestamp.
TIMESTAMP-STRING is in format '2025-06-22T22:43:06.866056Z'.
Returns an inactive timestamp like '[2025-06-22 Wed 22:43]' or nil if invalid."
  (when timestamp-string
    (condition-case err
        (let* ((cleaned-timestamp (replace-regexp-in-string "\\..*Z$" "Z" timestamp-string))
               (parsed-time (date-to-time cleaned-timestamp))
               (formatted-date (format-time-string "%Y-%m-%d %a %H:%M" parsed-time)))
          (format "[%s]" formatted-date))
      (error
       (orgist-log 'warn "Error parsing Todoist timestamp %s: %s"
                   timestamp-string (error-message-string err))
       nil))))

(defun orgist--id-label (id &optional name)
  "Return \"id[name]\" with NAME truncated to 20 chars."
  (if (and name (not (string-empty-p name)))
      (format "%s[%s]" (or id "?")
              (string-limit name 20))
    (or id "?")))

(defun orgist-log (level format-string &rest args)
  "Log message at LEVEL to *Messages* and to `orgist-log-file'.
The file always receives all levels; *Messages* respects `orgist-log-level'.
When `orgist--log-buffer' is non-nil, file writes are deferred."
  (let* ((priorities '((debug . 1) (info . 2) (warn . 3)))
         (text (apply #'format format-string args))
         (file-msg (format "Orgist [%s] %s"
                           (upcase (symbol-name level)) text))
         (echo-msg (if (eq level 'info)
                       (format "Orgist: %s" text)
                     (format "Orgist [%s] %s"
                             (upcase (symbol-name level)) text))))
    (when (>= (cdr (assq level priorities))
              (cdr (assq orgist-log-level priorities)))
      (message "%s" echo-msg))
    (when orgist-log-file
      (if orgist--log-buffer
          (push (concat file-msg "\n") orgist--log-buffer)
        (orgist--maybe-rotate-log)
        (let ((log-dir (file-name-directory orgist-log-file))
              (coding-system-for-write 'utf-8-unix))
          (unless (file-directory-p log-dir)
            (make-directory log-dir t))
          (write-region (concat file-msg "\n") nil orgist-log-file t 'silent))))))

(defun orgist-log-start-session ()
  "Write a session separator to the log file."
  (when orgist-log-file
    (let ((text (format "\n=== Orgist sync started at %s ===\n"
                        (format-time-string "%Y-%m-%d %H:%M:%S"))))
      (if orgist--log-buffer
          (push text orgist--log-buffer)
        (let ((log-dir (file-name-directory orgist-log-file))
              (coding-system-for-write 'utf-8-unix))
          (unless (file-directory-p log-dir)
            (make-directory log-dir t))
          (write-region text nil orgist-log-file t 'silent))))))

(defun orgist-delete-subtree ()
  "Delete the current subtree."
  (let (beg end)
    (org-back-to-heading t)
    (setq beg (point))
    (goto-char (orgist--subtree-end))
    ;; Include the end of an inlinetask
    (when (and (featurep 'org-inlinetask)
               (looking-at-p (concat (org-inlinetask-outline-regexp)
                                     "END[ \t]*$")))
      (end-of-line))
    (setq end (point))
    (delete-region beg end)))

(defun orgist--transplant-subtree (target-buffer)
  "Move the subtree at point into TARGET-BUFFER, preserving its text.
Used for cross-project moves so logbook history, CLOSED stamps and
local children survive instead of being recreated from Todoist data.
The subtree is deleted from the current buffer and appended to
TARGET-BUFFER as a top-level heading; the caller's reparent and
reposition steps then place it correctly.  Point must be on the
heading."
  (org-back-to-heading t)
  (let* ((beg (point))
         (end (orgist--subtree-end))
         (subtree (buffer-substring-no-properties beg end)))
    (delete-region beg end)
    (with-current-buffer target-buffer
      (save-excursion
        (goto-char (point-max))
        (unless (bolp) (insert "\n"))
        (let ((paste-pos (point)))
          (org-paste-subtree 1 subtree)
          (goto-char paste-pos)
          (org-back-to-heading t)
          (orgist-rebuild-subtree-cache))))))

(defun orgist-delete-element (element-id label)
  "Delete the org subtree for ELEMENT-ID in response to a remote deletion.
Todoist strips a deleted element's `project_id' to a placeholder, so the
owning project buffer can't be resolved from the element itself; instead
search every project file under `orgist-base-dir' for the heading that
carries ELEMENT-ID and delete its subtree there.  LABEL is used only for
logging.  Also drops the element's write-back snapshot so the now-removed
heading is not later mistaken for a local deletion to push back."
  (let ((deleted nil))
    (catch 'done
      (dolist (file (directory-files orgist-base-dir t "\\`[^.].*\\.org\\'"))
        (when-let* ((buffer (or (find-buffer-visiting file)
                                (find-file-noselect file))))
          (with-current-buffer buffer
            (save-excursion
              (when-let* ((point (orgist-find-element-by-id element-id)))
                (goto-char point)
                (orgist-delete-subtree)
                ;; Drop the now-stale cache entry; surviving markers track
                ;; the deletion automatically and self-heal on next lookup.
                (when orgist-id-cache
                  (remhash element-id orgist-id-cache))
                (orgist--save-buffer)
                (setq deleted t)
                (throw 'done nil)))))))
    (when orgist-snapshots
      (remhash element-id orgist-snapshots))
    (if deleted
        (orgist-log 'debug "Deleted element %s (remote deletion)" label)
      (orgist-log 'debug "Element %s already absent locally, nothing to delete"
                  label))))

;;; Write-back (local changes -> Todoist)

(defun orgist-snapshot-element (element)
  "Record ELEMENT's Todoist state in `orgist-snapshots'.
ELEMENT is the raw alist from the Todoist API response.
Reads scheduled/deadline from the org buffer (post-update) so
the snapshot matches what `orgist-element-local-state' will return."
  (unless orgist-snapshots
    (orgist-load-snapshots)
    (unless orgist-snapshots
      (setq orgist-snapshots (make-hash-table :test 'equal))))
  (let* ((id (alist-get 'id element))
         (name (alist-get 'name element))
         (checked (eq (alist-get 'checked element) t))
         (priority (alist-get 'priority element))
         (labels (mapcar #'orgist--label-to-tag
                         (append (alist-get 'labels element) nil)))
         ;; Read the org heading text (post-conversion), so the snapshot
         ;; matches what orgist-element-local-state will produce.
         (heading (car (orgist-extract-heading-and-tags)))
         (due (org-entry-get (point) "SCHEDULED"))
         (deadline (org-entry-get (point) "DEADLINE"))
         (description (orgist-extract-body-text))
         (parent-id (save-excursion
                      (org-back-to-heading t)
                      (if (org-up-heading-safe)
                          (org-entry-get (point) "ID")
                        (org-entry-get (point-min) "ID"))))
         (order (or (alist-get 'section_order element)
                    (alist-get 'child_order element)))
         (section-p (not (null name))))
    (let ((old-snap (gethash id orgist-snapshots))
          (duration (orgist-org-timestamp-extract-duration due)))
      ;; Instrumentation: any string field stored as unibyte with high
      ;; bytes will fail `equal' against the multibyte form read from
      ;; the live buffer, producing spurious write-back diffs.  We
      ;; can't currently reproduce the upstream cause, so log enough
      ;; runtime context to catch it the next time it happens.
      (orgist--snapshot-warn-unibyte
       id `((:content . ,heading)
            (:description . ,description)
            (:due . ,due)
            (:deadline . ,deadline)
            (:due-string . ,(org-entry-get (point) "TODOIST_DUE_STRING"))
            (:last-repeat . ,(org-entry-get (point) "LAST_REPEAT"))))
      (puthash id
               (list :content heading
                     :checked checked
                     :priority (or priority 1)
                     :labels labels
                     :due due
                     :due-string (org-entry-get (point) "TODOIST_DUE_STRING")
                     :deadline deadline
                     :duration duration
                     :description description
                     :parent-id parent-id
                     :order order
                     :section-p section-p
                     :archived-p (and section-p (eq (alist-get 'is_archived element) t))
                     :note-count (alist-get 'note_count element)
                     :responsible-uid (alist-get 'responsible_uid element)
                     :comment-ids (when old-snap
                                    (plist-get old-snap :comment-ids))
                     :activity-ids (when old-snap
                                     (plist-get old-snap :activity-ids))
                     :comments-pulled (when old-snap
                                        (plist-get old-snap :comments-pulled))
                     :reminder-ids (when old-snap
                                     (plist-get old-snap :reminder-ids))
                     :attachment-files (or (when old-snap
                                           (plist-get old-snap :attachment-files))
                                         (when orgist-sync-attachments
                                           (orgist--collect-attachment-files)))
                     :metadata-comment-id (when old-snap
                                            (plist-get old-snap :metadata-comment-id))
                     :last-repeat (org-entry-get (point) "LAST_REPEAT"))
               orgist-snapshots))))

(defun orgist--snapshot-warn-unibyte (id field-alist)
  "Log a warning if any (FIELD . VALUE) in FIELD-ALIST is a unibyte
string with high bytes — that's the failure mode that produces
spurious write-back diffs.  Captures buffer name, multibyte flag,
and process-coding so we can see what runtime state produced it."
  (dolist (cell field-alist)
    (let ((field (car cell)) (val (cdr cell)))
      (when (and (stringp val)
                 (not (multibyte-string-p val))
                 (string-match-p "[^\x00-\x7f]" val))
        (orgist-log 'warn
                    "UNIBYTE snapshot write: id=%s field=%s buf=%s buf-mb=%s file-cs=%s dpcs=%S snippet=%S"
                    id field
                    (buffer-name)
                    enable-multibyte-characters
                    buffer-file-coding-system
                    (default-value 'default-process-coding-system)
                    (substring val 0 (min 80 (length val))))))))

(defvar orgist--snapshot-count-on-disk nil
  "Number of snapshot entries last read from or written to disk.
Used as a safety check: if the in-memory count drops significantly
from this value, saving is refused to prevent data loss.")

(defun orgist-save-snapshots ()
  "Persist `orgist-snapshots' to `orgist-snapshot-file'.
Refuses to save if the entry count dropped by more than half
compared to the last known on-disk count (guards against saving
a partially-loaded hash that would overwrite good data)."
  (when orgist-snapshots
    (let ((new-count (hash-table-count orgist-snapshots))
          (dir (file-name-directory orgist-snapshot-file)))
      ;; When on-disk count is unknown (fresh session or package reload),
      ;; peek at the existing file to establish the baseline.  This
      ;; prevents a small in-memory hash from clobbering a large file.
      (when (and (not orgist--snapshot-count-on-disk)
                 (file-exists-p orgist-snapshot-file))
        (setq orgist--snapshot-count-on-disk
              (with-temp-buffer
                (insert-file-contents orgist-snapshot-file)
                (goto-char (point-min))
                (forward-line 1)
                (let ((entries (ignore-errors (read (current-buffer)))))
                  (length (or entries '()))))))
      (if (and orgist--snapshot-count-on-disk
               (> orgist--snapshot-count-on-disk 20)
               (< new-count (/ orgist--snapshot-count-on-disk 2)))
          ;; Safety: refuse to save a drastically smaller set.
          (orgist-log 'warn
                      "Snapshot save BLOCKED: count dropped from %d to %d (would lose data). Run M-x orgist-rebuild-snapshots to fix."
                      orgist--snapshot-count-on-disk new-count)
        (unless (file-directory-p dir)
          (make-directory dir t))
        (with-temp-file orgist-snapshot-file
          (insert ";; orgist snapshots -- do not edit\n")
          (let ((entries '())
                (print-length nil)
                (print-level nil))
            (maphash (lambda (id plist)
                       (push (cons id plist) entries))
                     orgist-snapshots)
            (prin1 entries (current-buffer))
            (insert "\n")))
        (setq orgist--snapshot-count-on-disk new-count)
        (orgist-log 'debug "Saved %d snapshots to %s"
                    new-count orgist-snapshot-file)))))

(defun orgist--snapshot-decode-tree (val)
  "Recursively decode unibyte UTF-8 strings to multibyte in VAL.
Octal-escape forms in the snapshot file (`\\NNN\\NNN\\NNN') are
read as unibyte strings whose raw bytes are the UTF-8 encoding of
the original character.  Comparing such a string with `equal'
against the multibyte form from a live buffer always returns nil,
producing spurious write-back diffs.  This walker decodes any
unibyte string containing high bytes back into multibyte form so
the in-memory hash is uniformly multibyte regardless of how a
given entry was last serialized."
  (cond
   ((and (stringp val)
         (not (multibyte-string-p val))
         (string-match-p "[^\x00-\x7f]" val))
    (decode-coding-string val 'utf-8))
   ((stringp val) val)
   ((consp val)
    (cons (orgist--snapshot-decode-tree (car val))
          (orgist--snapshot-decode-tree (cdr val))))
   (t val)))

(defun orgist-load-snapshots (&optional force)
  "Load `orgist-snapshots' from `orgist-snapshot-file'.
Skips loading if snapshots are already in memory, unless FORCE is non-nil.
Also loads `orgist-labels' from `orgist-labels-file' when not already set."
  (orgist-load-labels)
  (when (or force (not orgist-snapshots))
    (setq orgist-snapshots (make-hash-table :test 'equal))
    (if (file-exists-p orgist-snapshot-file)
        (progn
          (with-temp-buffer
            (insert-file-contents orgist-snapshot-file)
            (goto-char (point-min))
            ;; Skip comment line
            (forward-line 1)
            (let ((entries (read (current-buffer)))
                  (fixed 0))
              (dolist (entry entries)
                (let* ((raw (cdr entry))
                       (decoded (orgist--snapshot-decode-tree raw)))
                  (unless (eq raw decoded) (cl-incf fixed))
                  (puthash (car entry) decoded orgist-snapshots)))
              (when (> fixed 0)
                (orgist-log 'debug "Decoded %d unibyte snapshot entries to multibyte"
                            fixed))))
          (setq orgist--snapshot-count-on-disk (hash-table-count orgist-snapshots))
          (orgist-log 'debug "Loaded %d snapshots from %s"
                      orgist--snapshot-count-on-disk
                      orgist-snapshot-file))
      ;; No file — reset the on-disk count so the save guard
      ;; doesn't compare against a stale value.
      (setq orgist--snapshot-count-on-disk nil))))

;;; Write-back verification stamps
;;
;; Stamps decouple "this file's local changes were diffed and
;; dispatched" from "snapshots were persisted".  The previous design
;; compared file mtimes against the snapshot file's mtime, but any
;; pull re-saves snapshots, so a pull landing after a failed
;; write-back would permanently hide the pending local changes
;; (observed 2026-08-12: the mask engaged one second after a save).
;; A stamp only advances when a scan of that file completed without
;; element errors AND its result was resolved — nothing to push,
;; commands executed successfully, or a dry run that advanced
;; snapshots.  A failed scan, a failed API call, or a cancelled
;; confirmation leaves the stamp behind, so the next save or sync
;; automatically retries.

(defun orgist--stamps-path ()
  "Return the write-back stamps file path.
Each stamp records the content hash a project file had when a
write-back diff scan of that file last ran to completion and its
outcome was resolved (commands executed, or nothing to push).
The file is co-located with `orgist-snapshot-file' so that any
sandboxing that redirects snapshots (tests, alternate data dirs)
redirects stamps with it — but it is a separate file on purpose:
snapshots may be re-persisted by any pull, and that must never
hide local changes from the diff scan."
  (expand-file-name "write-back-stamps.el"
                    (file-name-directory
                     (expand-file-name orgist-snapshot-file))))

(defun orgist--file-content-hash (file)
  "Return the SHA-1 of FILE's bytes on disk, or nil if unreadable."
  (when (file-readable-p file)
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (insert-file-contents-literally file)
      (secure-hash 'sha1 (current-buffer)))))

(defun orgist--load-stamps (&optional force)
  "Load `orgist--write-back-stamps' from `orgist--stamps-path'.
Skips loading if stamps are already in memory for the current
path, unless FORCE is non-nil.  A missing file yields an empty
table, which marks every project file as due for scanning
\(correct for a first run)."
  (let ((path (orgist--stamps-path)))
    (when (or force
              (not orgist--write-back-stamps)
              (not (equal path orgist--write-back-stamps-path)))
      (setq orgist--write-back-stamps (make-hash-table :test 'equal))
      (setq orgist--write-back-stamps-path path)
      (when (file-exists-p path)
        (with-temp-buffer
          (insert-file-contents path)
          (goto-char (point-min))
          (forward-line 1)              ; skip comment line
          (dolist (entry (ignore-errors (read (current-buffer))))
            (puthash (car entry) (cdr entry) orgist--write-back-stamps)))))))

(defun orgist--save-stamps ()
  "Persist `orgist--write-back-stamps' to `orgist--stamps-path'."
  (when orgist--write-back-stamps
    (let* ((path (orgist--stamps-path))
           (dir (file-name-directory path)))
      (unless (file-directory-p dir)
        (make-directory dir t))
      (with-temp-file path
        (insert ";; orgist write-back stamps -- do not edit\n")
        (let ((entries '())
              (print-length nil)
              (print-level nil))
          (maphash (lambda (file hash) (push (cons file hash) entries))
                   orgist--write-back-stamps)
          (prin1 entries (current-buffer))
          (insert "\n"))))))

(defun orgist--stamp-file (file hash)
  "Record HASH as FILE's verified content hash."
  (orgist--load-stamps)
  (puthash file hash orgist--write-back-stamps))

(defun orgist--commit-pending-stamps ()
  "Commit `orgist--pending-stamps' and persist the stamp table.
Called after write-back commands were all executed successfully
\(or advanced snapshots in dry-run mode)."
  (when orgist--pending-stamps
    (dolist (entry orgist--pending-stamps)
      (orgist--stamp-file (car entry) (cdr entry)))
    (setq orgist--pending-stamps nil)
    (orgist--save-stamps)))

(defun orgist-rebuild-snapshots ()
  "Rebuild snapshots from current org buffer content.
Walk all orgist-managed buffers and capture the local state as the
baseline for write-back diffing.  Metadata fields (note-count,
comment-ids, etc.) are preserved from existing snapshots.  Snapshot
entries whose ID no longer appears in any org file are purged, so
stale temp-ids from failed item_add commands don't resurface as
phantom deletions on the next write-back.
Useful after a sync error that left snapshots stale."
  (interactive)
  ;; Start from on-disk snapshots so metadata fields are preserved.
  (orgist-load-snapshots t)
  (let ((count 0)
        (seen-ids (make-hash-table :test 'equal)))
    (dolist (file (directory-files orgist-base-dir t "\\`[^.].*\\.org\\'"))
      (let ((buf (or (find-buffer-visiting file)
                     (find-file-noselect file))))
        (with-current-buffer buf
          (org-with-wide-buffer
           (goto-char (point-min))
           (while (re-search-forward "^\\*+ " nil t)
             (org-back-to-heading t)
             (when-let* ((id (org-entry-get (point) "ID")))
               (let* ((local (orgist-element-local-state))
                      (old (gethash id orgist-snapshots)))
                 ;; Preserve metadata from the existing snapshot
                 (when old
                   (dolist (key '(:note-count :responsible-uid
                                  :comment-ids :activity-ids
                                  :comments-pulled :reminder-ids
                                  :activity-note-count :due-string))
                     (when-let* ((val (plist-get old key)))
                       (setq local (plist-put local key val)))))
                 ;; Synthesize metadata from existing logbook Notes
                 ;; when missing, so the diff doesn't treat them as
                 ;; newly added and comments subprocess doesn't
                 ;; re-pull everything.
                 (when orgist-sync-comments
                   (unless (plist-get local :comment-ids)
                     (let ((n (length (orgist-extract-logbook-notes))))
                       (when (> n 0)
                         (setq local
                               (plist-put local :comment-ids
                                          (make-list n "rebuilt"))))))
                   (unless (plist-get local :comments-pulled)
                     (setq local (plist-put local :comments-pulled t)))
                   ;; Normalize :attachment-files from flat list
                   ;; ("file.png" ...) to alist format
                   ;; ((nil . "file.png") ...) expected by the
                   ;; comments subprocess.  Also migrate old format
                   ;; (("file.png" . cid) ...) to new (cid . "file").
                   (let ((afiles (plist-get local :attachment-files)))
                     (when afiles
                       (cond
                        ;; Flat list of strings → wrap
                        ((stringp (car afiles))
                         (setq local
                               (plist-put local :attachment-files
                                          (mapcar (lambda (f) (cons nil f))
                                                  afiles))))
                        ;; Old format: car is a filename string, not
                        ;; a comment-id.  Comment-ids are numeric
                        ;; strings; filenames have dots/extensions.
                        ((and (consp (car afiles))
                              (stringp (caar afiles))
                              (string-match-p "\\." (caar afiles)))
                         (setq local
                               (plist-put local :attachment-files
                                          (mapcar (lambda (e)
                                                    (cons (cdr e) (car e)))
                                                  afiles))))))))
                 (puthash id local orgist-snapshots)
                 (puthash id t seen-ids)
                 (cl-incf count)))
             (outline-next-heading))))))
    ;; Purge orphans: snapshot entries whose ID is not present in any
    ;; org file.  Common cause is a temp-id from a failed item_add that
    ;; was stored by `orgist-update-snapshots-from-local' but never
    ;; remapped to a real Todoist ID.
    (let ((purged 0)
          (to-remove '()))
      (maphash (lambda (id _snap)
                 (unless (gethash id seen-ids)
                   (push id to-remove)))
               orgist-snapshots)
      (dolist (id to-remove)
        (remhash id orgist-snapshots)
        (cl-incf purged))
      (when (> purged 0)
        (orgist-log 'info "Rebuilt snapshots: purged %d orphan(s)" purged)))
    (orgist-save-snapshots)
    (orgist-log 'info "Rebuilt %d snapshots from local state" count)
    (message "Orgist: rebuilt %d snapshots" count)))

(defun orgist-validate-snapshots ()
  "Check snapshot integrity against org files.
Reports tasks missing from snapshots, orphaned snapshot entries,
and offers to rebuild when problems are found."
  (interactive)
  (orgist-load-snapshots)
  (let ((org-ids (make-hash-table :test 'equal))
        (missing '())
        (orphaned '())
        (total-org 0)
        (total-snap (hash-table-count orgist-snapshots)))
    ;; Collect all task/section IDs from org files.
    (dolist (file (directory-files orgist-base-dir t "\\`[^.].*\\.org\\'"))
      (let ((buf (or (find-buffer-visiting file)
                     (find-file-noselect file))))
        (with-current-buffer buf
          (org-with-wide-buffer
           (goto-char (point-min))
           (while (re-search-forward "^\\*+ " nil t)
             (org-back-to-heading t)
             (when-let* ((id (org-entry-get (point) "ID")))
               ;; Skip file-level project IDs (they don't get snapshots)
               (unless (= (point) (point-min))
                 (puthash id (file-name-nondirectory
                              (buffer-file-name)) org-ids)
                 (cl-incf total-org)))
             (outline-next-heading))))))
    ;; Find org tasks missing from snapshots.
    (maphash (lambda (id file)
               (unless (gethash id orgist-snapshots)
                 (push (cons id file) missing)))
             org-ids)
    ;; Find orphaned snapshot entries (not in any org file).
    (maphash (lambda (id _snap)
               (unless (gethash id org-ids)
                 (push id orphaned)))
             orgist-snapshots)
    ;; Report results.
    (if (and (null missing) (null orphaned))
        (message "Orgist: snapshots OK — %d entries match %d org headings"
                 total-snap total-org)
      (let ((buf (get-buffer-create "*Orgist Snapshot Validation*")))
        (with-current-buffer buf
          (let ((inhibit-read-only t))
            (erase-buffer)
            (insert (format "Orgist Snapshot Validation\n\n"))
            (insert (format "Org headings with ID: %d\n" total-org))
            (insert (format "Snapshot entries:     %d\n\n" total-snap))
            (when missing
              (insert (format "== %d task(s) MISSING from snapshots ==\n"
                              (length missing)))
              (insert "These tasks are invisible to write-back detection.\n\n")
              (dolist (entry (seq-take (sort missing
                                            (lambda (a b)
                                              (string< (cdr a) (cdr b))))
                                       50))
                (insert (format "  %s  (%s)\n" (car entry) (cdr entry))))
              (when (length> missing 50)
                (insert (format "  ... and %d more\n"
                                (- (length missing) 50))))
              (insert "\n"))
            (when orphaned
              (insert (format "== %d orphaned snapshot(s) ==\n"
                              (length orphaned)))
              (insert "Snapshot entries with no matching org heading.\n\n")
              (dolist (id (seq-take orphaned 20))
                (let ((snap (gethash id orgist-snapshots)))
                  (insert (format "  %s  %s\n" id
                                  (or (plist-get snap :content) "?")))))
              (when (length> orphaned 20)
                (insert (format "  ... and %d more\n"
                                (- (length orphaned) 20))))
              (insert "\n"))
            (insert "Run M-x orgist-rebuild-snapshots to fix.\n"))
          (goto-char (point-min))
          (special-mode))
        (display-buffer buf)
        (when (and missing
                   (yes-or-no-p
                    (format "Orgist: %d task(s) missing from snapshots. Rebuild now? "
                            (length missing))))
          (orgist-rebuild-snapshots))))))

(defun orgist-extract-heading-and-tags ()
  "Parse heading text and trailing tags from the current line.
Returns (HEADING . TAGS) where HEADING is the cleaned text and
TAGS is a list of tag strings.  Parses the raw buffer line to
handle tags with hyphens that `org-get-tags' would miss."
  (let* ((raw-line (buffer-substring-no-properties
                    (line-beginning-position) (line-end-position)))
         ;; Build regex from all known TODO keywords (not just TODO/DONE)
         (kw-re (if (bound-and-true-p org-todo-keywords-1)
                    (concat "\\(?:"
                            (mapconcat #'regexp-quote org-todo-keywords-1 "\\|")
                            "\\)")
                  "\\(?:TODO\\|DONE\\)"))
         ;; Strip leading stars, TODO keyword, and priority cookie
         (content (if (string-match
                       (concat "^\\*+ +\\(?:" kw-re " \\)?\\(?:\\[#.\\] \\)?")
                       raw-line)
                      (substring raw-line (match-end 0))
                    raw-line)))
    (if (string-match
         "\\(.*\\) +:\\([^ \t:]+\\(?::[^ \t:]+\\)*\\): *$"
         content)
        (let ((heading-part (match-string 1 content))
              (tag-part (match-string 2 content)))
          (cons (string-trim heading-part)
                (split-string tag-part ":")))
      (cons (string-trim content) nil))))

(defun orgist-org-timestamp-to-date (timestamp)
  "Extract the YYYY-MM-DD date string from an org TIMESTAMP.
Strips day name, time, repeaters, and angle brackets.
Returns nil if TIMESTAMP is nil."
  (when timestamp
    (if (string-match "<\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\)" timestamp)
        (match-string 1 timestamp)
      timestamp)))

(defun orgist-org-timestamp-to-todoist-due (timestamp &optional stored-due-string)
  "Convert an org TIMESTAMP string to a Todoist due object.
TIMESTAMP is like \"<2026-02-17 Tue 06:30 +7d>\" or \"<2026-02-20 Fri>\".
STORED-DUE-STRING, when non-nil, is the original Todoist recurrence
string (e.g. \"every mon at 6:30am\") preserved from the last pull.
It is used instead of synthesizing a new string from the repeater,
so that day-of-week anchored recurrences survive a reschedule.
Returns an alist suitable for the Todoist API `due' field, with
`date' (ISO format), `is_recurring', and `string' keys."
  (when timestamp
    (let* ((date (orgist-org-timestamp-to-date timestamp))
           ;; Extract start time (HH:MM), excluding time ranges
           (time (when (string-match
                        "<[0-9-]+ [A-Za-z]+ \\([0-9]\\{1,2\\}:[0-9]\\{2\\}\\)"
                        timestamp)
                   (match-string 1 timestamp)))
           ;; Extract repeater (+Nd, ++1w, .+2m, etc.)
           (repeater (when (string-match
                            "\\([.+]+[0-9]+[dwmy]\\)"
                            timestamp)
                       (match-string 1 timestamp)))
           ;; Build the date field (YYYY-MM-DD or YYYY-MM-DDTHH:MM:SS)
           (api-date (if time
                        (format "%sT%s:00" date time)
                      date))
           ;; Build a human-readable string for the Todoist API.
           ;; Prefer the stored Todoist string when the timestamp still
           ;; has a repeater — it preserves day-of-week anchors that
           ;; the org repeater syntax cannot encode.
           (is-recurring (not (null repeater)))
           (date-string (if (and stored-due-string is-recurring)
                            stored-due-string
                          (orgist-org-timestamp-to-string
                           date time repeater))))
      (let ((result (list (cons 'date api-date))))
        (when date-string
          (push (cons 'string date-string) result))
        (push (cons 'is_recurring is-recurring) result)
        result))))

(defun orgist-org-timestamp-extract-duration (timestamp)
  "Extract duration from an org TIMESTAMP with a time range.
TIMESTAMP is like \"<2026-02-20 Fri 13:00-14:00>\".
Returns a Todoist duration alist ((amount . N) (unit . \"minute\"))
or nil if no time range is present."
  (when (and timestamp
             (string-match
              "\\([0-9]\\{1,2\\}:[0-9]\\{2\\}\\)-\\([0-9]\\{1,2\\}:[0-9]\\{2\\}\\)"
              timestamp))
    (let* ((start-str (match-string 1 timestamp))
           (end-str (match-string 2 timestamp))
           (start-parts (split-string start-str ":"))
           (end-parts (split-string end-str ":"))
           (start-mins (+ (* (string-to-number (car start-parts)) 60)
                          (string-to-number (cadr start-parts))))
           (end-mins (+ (* (string-to-number (car end-parts)) 60)
                        (string-to-number (cadr end-parts))))
           (duration-mins (- end-mins start-mins)))
      (when (> duration-mins 0)
        (list (cons 'amount duration-mins)
              (cons 'unit "minute"))))))

(defun orgist--extract-repeater (timestamp)
  "Extract the repeater portion from an org TIMESTAMP string.
Returns a string like \"++1w\" or \".+2d\", or nil if TIMESTAMP
has no repeater or is nil."
  (when (and timestamp
             (string-match "\\([.+]+[0-9]+[dwmyh]\\)" timestamp))
    (match-string 1 timestamp)))

(defun orgist-org-timestamp-to-string (date time repeater)
  "Build a Todoist-style human-readable date string.
DATE is YYYY-MM-DD, TIME is HH:MM or nil, REPEATER is like \"+7d\" or nil."
  (let* ((parts '())
         ;; Parse date to get month/day
         (date-parts (split-string date "-"))
         (year (string-to-number (nth 0 date-parts)))
         (month (string-to-number (nth 1 date-parts)))
         (day (string-to-number (nth 2 date-parts)))
         (month-names ["Jan" "Feb" "Mar" "Apr" "May" "Jun"
                       "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"]))
    ;; Date part — emit only for non-recurring tasks.  For recurring
    ;; tasks, Todoist's NLP can re-interpret an explicit-date prefix
    ;; ("May 8 2026 every! 3 days") as a one-time date on item_update,
    ;; losing the recurrence.  The structured `date' field on the API
    ;; payload carries the date independently of `string'.
    (unless repeater
      (push (format "%s %d %d" (aref month-names (1- month)) day year) parts))
    ;; Time part
    (when time
      (let* ((time-parts (split-string time ":"))
             (hour (string-to-number (car time-parts)))
             (minute (string-to-number (cadr time-parts)))
             (ampm (if (>= hour 12) "pm" "am"))
             (hour12 (cond ((= hour 0) 12)
                           ((> hour 12) (- hour 12))
                           (t hour))))
        (push (if (= minute 0)
                  (format "at %d%s" hour12 ampm)
                (format "at %d:%02d%s" hour12 minute ampm))
              parts)))
    ;; Repeater part
    (when repeater
      (when (string-match "\\([.+]+\\)\\([0-9]+\\)\\([dwmy]\\)" repeater)
        (let* ((prefix (match-string 1 repeater))
               (count (string-to-number (match-string 2 repeater)))
               (unit-char (match-string 3 repeater))
               (unit-name (cond
                           ((string= unit-char "d") (if (= count 1) "day" "days"))
                           ((string= unit-char "w") (if (= count 1) "week" "weeks"))
                           ((string= unit-char "m") (if (= count 1) "month" "months"))
                           ((string= unit-char "y") (if (= count 1) "year" "years"))
                           (t unit-char)))
               ;; `.+' is Todoist's completion-based recurrence ("every!"),
               ;; inverse of the parse in `orgist-parse-recurrence-string'.
               (every (if (string= prefix ".+") "every!" "every")))
          (if (= count 1)
              (push (format "%s %s" every unit-name) parts)
            (push (format "%s %d %s" every count unit-name) parts)))))
    (string-join (nreverse parts) " ")))

(defun orgist--find-same-type-neighbor-order (direction is-section)
  "Find the TODOIST-ORDER of the nearest same-type sibling.
DIRECTION is `prev' or `next'.  IS-SECTION is non-nil for sections.
Point must be on the heading.  Returns the order number or nil."
  (let ((move-fn (if (eq direction 'prev)
                     #'org-get-previous-sibling
                   #'org-get-next-sibling))
        (result nil))
    (while (and (not result) (funcall move-fn))
      (let ((sib-section (not (null (org-entry-get (point) "SECTION")))))
        (when (eq (not (not sib-section)) (not (not is-section)))
          (let ((o (org-entry-get (point) "TODOIST-ORDER")))
            (when o (setq result (string-to-number o)))))))
    result))

(defun orgist--collect-direct-children (parent-level)
  "Collect direct children at PARENT-LEVEL + 1 from point.
Point should be on the parent heading.  Returns a list of
\(ID ORDER IS-SECTION POINT) in buffer order for children
that have both ID and TODOIST-ORDER properties."
  (let ((child-level (1+ parent-level))
        (subtree-end (orgist--subtree-end))
        (children '()))
    (save-excursion
      (forward-line 1)
      (while (re-search-forward org-outline-regexp-bol subtree-end t)
        (let ((level (org-current-level)))
          (cond
           ((= level child-level)
            (let ((id (org-entry-get (point) "ID"))
                  (order-str (org-entry-get (point) "TODOIST-ORDER"))
                  (sec (not (null (org-entry-get (point) "SECTION")))))
              (when (and id order-str)
                (push (list id (string-to-number order-str) sec (point))
                      children))))
           ;; Skip deeper levels
           ((> level child-level) nil)
           ;; Stop if we reach same or higher level
           ((< level child-level) (goto-char subtree-end))))))
    (nreverse children)))

(defun orgist--position-aware-order ()
  "Return the effective order of the heading at point.
Reads TODOIST-ORDER and checks whether the heading's rank among
its same-type siblings (by buffer position) matches the rank
implied by TODOIST-ORDER values.  When they differ, the user has
moved the heading and a new order value is computed.
Collects siblings by scanning the parent's direct children
rather than using `org-get-previous-sibling' (which has edge
cases with parent boundaries)."
  (let ((stored-str (org-entry-get (point) "TODOIST-ORDER")))
    (when stored-str
      (let* ((stored (string-to-number stored-str))
             (is-section (not (null (org-entry-get (point) "SECTION"))))
             (my-id (org-entry-get (point) "ID"))
             ;; Go to parent and collect children
             (parent-level (save-excursion
                             (if (org-up-heading-safe) (org-current-level) 0)))
             (all-children (save-excursion
                             (if (> parent-level 0)
                                 (progn (org-up-heading-safe)
                                        (orgist--collect-direct-children parent-level))
                               ;; Top-level: scan from buffer start
                               (goto-char (point-min))
                               (orgist--collect-direct-children 0))))
             ;; Filter to same type (items vs sections)
             (siblings (seq-filter
                        (lambda (c) (eq (nth 2 c) is-section))
                        all-children)))
        ;; Find this element's rank in buffer order vs order-sorted rank
        (let* ((buffer-rank (cl-position my-id siblings
                                         :key #'car :test #'equal))
               (order-sorted (sort siblings :key #'cadr))
               (order-rank (cl-position my-id order-sorted
                                        :key #'car :test #'equal)))
          (if (or (null buffer-rank) (null order-rank)
                  (= buffer-rank order-rank))
              stored
            ;; Ranks differ — compute new order from buffer neighbors
            (let* ((prev-sib (when (> buffer-rank 0)
                               (nth (1- buffer-rank) siblings)))
                   (next-sib (when (< buffer-rank (1- (length siblings)))
                               (nth (1+ buffer-rank) siblings)))
                   (prev-order (when prev-sib (nth 1 prev-sib)))
                   (next-order (when next-sib (nth 1 next-sib))))
              (cond
               ((and prev-order next-order (< prev-order next-order))
                (let ((mid (+ prev-order (/ (- next-order prev-order) 2))))
                  (if (< prev-order mid next-order)
                      mid
                    (1+ prev-order))))
               (prev-order (1+ prev-order))
               (next-order (max 0 (1- next-order)))
               (t stored)))))))))

(defun orgist-element-local-state ()
  "Extract the current heading's state as a plist.
Point must be on the heading.  Returns a plist with the same keys
as stored by `orgist-snapshot-element'."
  (org-back-to-heading-or-point-min t)
  (let* ((heading-tags (orgist-extract-heading-and-tags))
         (heading (car heading-tags))
         (labels (seq-remove (lambda (tag) (member tag '("ATTACH" "ARCHIVE")))
                             (cdr heading-tags)))
         (todo-state (org-get-todo-state))
         (checked (not (null (member todo-state org-done-keywords))))
         (priority (orgist-org-priority-to-todoist
                    (org-element-property :priority (org-element-at-point))))
         (due (org-entry-get (point) "SCHEDULED"))
         (deadline (org-entry-get (point) "DEADLINE"))
         (duration (orgist-org-timestamp-extract-duration due))
         (description (orgist-extract-body-text))
         (parent-id (save-excursion
                      (if (org-up-heading-safe)
                          (org-entry-get (point) "ID")
                        (org-entry-get (point-min) "ID"))))
         (order (let ((o (org-entry-get (point) "TODOIST-ORDER")))
                  (when o (string-to-number o))))
         (section-p (not (null (org-entry-get (point) "SECTION"))))
         (archived-p (and section-p
                          (not (null (member "ARCHIVE" (cdr heading-tags))))))
         (last-repeat (org-entry-get (point) "LAST_REPEAT"))
         (due-string (org-entry-get (point) "TODOIST_DUE_STRING")))
    (list :content heading
          :checked checked
          :priority priority
          :labels labels
          :due due
          :due-string due-string
          :deadline deadline
          :duration duration
          :description description
          :parent-id parent-id
          :order order
          :section-p section-p
          :archived-p archived-p
          :last-repeat last-repeat
          :attachment-files (when orgist-sync-attachments
                              (orgist--collect-attachment-files)))))

(defun orgist--collect-attachment-files ()
  "Collect attachment files from the current heading and non-TODO children.
Point must be on the heading.  Returns a sorted list of filenames,
or nil.  Files from non-TODO child headings (which have no Todoist
ID) are included so they can be pushed to the parent task."
  (let ((files '()))
    ;; Files on this heading
    (let ((dir (org-attach-dir nil t)))
      (when (and dir (file-directory-p dir))
        (setq files (org-attach-file-list dir))))
    ;; Files on non-TODO child headings (no Todoist ID of their own)
    (save-excursion
      (let ((subtree-end (orgist--subtree-end))
            (level (org-current-level)))
        (while (and (outline-next-heading)
                    (< (point) subtree-end))
          ;; Only direct children (one level deeper), not deeper descendants
          ;; with their own TODO parent.
          (when (and (= (org-current-level) (1+ level))
                     (not (org-get-todo-state)))
            (let ((dir (org-attach-dir nil t)))
              (when (and dir (file-directory-p dir))
                (dolist (f (org-attach-file-list dir))
                  (unless (member f files)
                    (push f files)))))))))
    (when files (sort files #'string<))))

(defun orgist--unique-attachment-name (dir base-name)
  "Return a filename in DIR that does not collide with an existing file.
Returns BASE-NAME if the path is free; otherwise appends _2, _3, etc."
  (if (not (file-exists-p (expand-file-name base-name dir)))
      base-name
    (let* ((ext (file-name-extension base-name t))
           (stem (file-name-sans-extension base-name))
           (n 2))
      (while (file-exists-p (expand-file-name (format "%s_%d%s" stem n ext) dir))
        (setq n (1+ n)))
      (format "%s_%d%s" stem n ext))))

(defun orgist-org-priority-to-todoist (priority-char)
  "Convert org priority character to Todoist priority (1-4).
PRIORITY-CHAR is the value from `org-element-property' :priority,
which is always a character code (e.g. 50 for [#2], 66 for [#B]),
or nil when no priority is set.  Reverses `orgist-todoist-priority-to-org'
using the current `org-priority-highest' setting."
  (if (null priority-char)
      1
    ;; org-element-property :priority always returns char codes (e.g. ?2 = 50),
    ;; but org-priority-highest may be a small integer (e.g. 1) when using
    ;; numeric priorities.  Normalize priority-char to match org-priority-highest's
    ;; domain: if org-priority-highest is below the ASCII digit range, convert
    ;; the char code to its digit value.
    (let* ((p (if (and (< org-priority-highest ?0) (>= priority-char ?0))
                  (- priority-char ?0)
                priority-char))
           (todoist (- 4 (- p org-priority-highest))))
      (max 1 (min 4 todoist)))))

(defun orgist-extract-body-text ()
  "Extract body text of the current heading (description content).
Returns text between the end of meta-data and the first child
heading, excluding state-change log entries (both at the start
and end of the body)."
  (save-excursion
    (org-back-to-heading-or-point-min t)
    (org-end-of-meta-data t)
    (let ((start (point))
          (end (save-excursion
                 (let ((subtree-end (orgist--subtree-end)))
                   (if (re-search-forward org-outline-regexp-bol subtree-end t)
                       (line-beginning-position)
                     subtree-end)))))
      (let ((log-re "^[ \t]*- \\(?:State\\|to\\|From\\) .+\n"))
        (thread-last (buffer-substring-no-properties start end)
          ;; Strip bare logbook lines (not inside :LOGBOOK: drawer)
          ;; from both the end and the start of the body text.
          (replace-regexp-in-string (concat "\\(?:" log-re "\\)+\\'") "")
          (replace-regexp-in-string (concat "\\`\\(?:" log-re "\\)+") "")
          string-trim)))))

(defun orgist-diff-element (element-id)
  "Compare local state of ELEMENT-ID against its snapshot.
Returns nil if unchanged, the symbol `deleted' if the element is
missing from the buffer, `new' if it has no snapshot, or an alist
of (FIELD . (OLD . NEW)) for each changed field."
  (let ((snapshot (gethash element-id orgist-snapshots)))
    (if (not snapshot)
        'new
      (let ((pos (orgist-find-element-by-id element-id)))
        (if (not pos)
            'deleted
          (save-excursion
            (goto-char pos)
            (let* ((local (orgist-element-local-state))
                   (fields `(:content :checked :priority :labels
                             :due :due-string :deadline :duration :description
                             :parent-id :order :section-p :archived-p :last-repeat
                             ,@(when orgist-sync-attachments
                                 '(:attachment-files))))
                   (diffs '()))
              (dolist (field fields)
                (let ((old-val (plist-get snapshot field))
                      (new-val (plist-get local field)))
                  ;; Normalize old snapshots that stored only YYYY-MM-DD
                  ;; to nil when local has nil (both mean "no date").
                  ;; New snapshots store the full org timestamp.
                  ;; Normalize descriptions: treat nil and empty/whitespace-only
                  ;; as equivalent, and trim before comparing to avoid spurious
                  ;; diffs from trailing whitespace or newline differences.
                  (when (eq field :description)
                    (setq old-val (and old-val
                                       (let ((s (string-trim old-val)))
                                         (unless (string-empty-p s) s))))
                    (setq new-val (and new-val
                                       (let ((s (string-trim new-val)))
                                         (unless (string-empty-p s) s)))))
                  ;; Normalize attachment-files: snapshot stores alist
                  ;; of (comment-id . file) pairs, local state stores
                  ;; flat filename list.  Compare only the filenames.
                  (when (eq field :attachment-files)
                    (let ((old-names (sort (mapcar (lambda (e)
                                                    (if (consp e) (cdr e) e))
                                                  (or old-val '()))
                                          #'string<))
                          (new-names (sort (or new-val '()) :lessp #'string<)))
                      (if (equal old-names new-names)
                          ;; No real change — skip this field
                          (setq old-val nil new-val nil)
                        ;; Real change — store filenames for the diff,
                        ;; command generator uses snapshot alist separately
                        (setq old-val old-names
                              new-val new-names))))
                  ;; Note: recurring timestamps (with repeaters like +1d)
                  ;; are NOT suppressed — date changes on repeating tasks
                  ;; are treated as intentional edits and written back.
                  (unless (equal old-val new-val)
                    (push (cons field (cons old-val new-val)) diffs))))
              ;; Detect new logbook Notes (for comment push).
              ;; Compare local Note count against known comment-ids,
              ;; excluding the metadata comment (no logbook note) and
              ;; "rebuilt" placeholders (set during snapshot rebuild,
              ;; not real Todoist IDs).
              (when orgist-sync-comments
                ;; Compare unique notes only — stale duplicates in the
                ;; logbook (from a pull re-insert after snapshot loss)
                ;; must never be pushed as new comments.  "rebuilt"
                ;; placeholders each stand in for one existing note
                ;; (see `orgist-rebuild-snapshots'), so they count as
                ;; known; only the metadata comment has no note.
                (let* ((local-notes (seq-uniq (orgist-extract-logbook-notes)
                                              #'equal))
                       (known-ids (or (plist-get snapshot :comment-ids) '()))
                       (metadata-id (plist-get snapshot :metadata-comment-id))
                       (content-ids
                        (seq-remove (lambda (id) (equal id metadata-id))
                                    known-ids))
                       (known-count (length content-ids)))
                  (when (> (length local-notes) known-count)
                    (push (cons :notes
                                (cons known-count (length local-notes)))
                          diffs))))
              ;; When :last-repeat is newly set (repeating task
              ;; instance completed), suppress :due and :duration from
              ;; the diff — Todoist advances the date itself on
              ;; item_close and the org-computed next date is
              ;; unreliable (e.g. ++1w from today vs. "every mon"
              ;; anchor).
              ;; Exception: if the repeater PREFIX changed (e.g. +1w →
              ;; .+1w), the user is intentionally switching recurrence
              ;; semantics.  Keep :due so command generation can send
              ;; the new recurrence string (date stripped there).
              (when (and (assq :last-repeat diffs)
                         (cddr (assq :last-repeat diffs)))  ; new-val non-nil
                (let* ((due-diff (assq :due diffs))
                       (old-rep (and due-diff
                                     (orgist--extract-repeater (cadr due-diff))))
                       (new-rep (and due-diff
                                     (orgist--extract-repeater (cddr due-diff))))
                       (prefix-changed
                        (and old-rep new-rep
                             (string-match "^\\([.+]+\\)" old-rep)
                             (let ((p (match-string 1 old-rep)))
                               (not (and (string-match "^\\([.+]+\\)" new-rep)
                                         (string= p (match-string 1 new-rep))))))))
                  (unless prefix-changed
                    (setq diffs (seq-remove
                                 (lambda (d) (memq (car d) '(:due :duration)))
                                 diffs)))))
              (when diffs (nreverse diffs)))))))))

(defun orgist--modified-org-files ()
  "Return the org files in `orgist-base-dir' due for a write-back scan.
A file is due when its on-disk content hash differs from its
verification stamp (see `orgist--stamps-path'), or when its
visiting buffer has unsaved modifications.  Files without a stamp
\(first run, or a scan that never completed) are always due.
Returns an alist of (FILE . HASH) so the scan can stamp the exact
content it verified; HASH is nil when only the buffer is modified."
  (orgist--load-stamps)
  (let ((due '()))
    (dolist (file (directory-files orgist-base-dir t "\\`[^.].*\\.org\\'"))
      (let* ((buf (find-buffer-visiting file))
             (buffer-dirty (and buf (buffer-modified-p buf)))
             (hash (unless buffer-dirty (orgist--file-content-hash file))))
        (when (or buffer-dirty
                  (not (equal hash (gethash file orgist--write-back-stamps))))
          (push (cons file (and (not buffer-dirty) hash)) due))))
    (nreverse due)))

(cl-defun orgist-diff-all-elements ()
  "Diff all known elements across all project buffers.
Returns a list of (ELEMENT-ID . DIFF) pairs where DIFF is the
result of `orgist-diff-element'.

Only diffs elements in files due for scanning (see
`orgist--modified-org-files').  Files that scan cleanly with no
changes are stamped as verified immediately; files with changes
are recorded in `orgist--pending-stamps' and stamped only when
their commands go through (`orgist--commit-pending-stamps').
A single element error never aborts the batch: the error is
logged, the file is left unstamped so the next scan retries it,
and every other element and file is still processed."
  (unless orgist-snapshots
    (orgist-load-snapshots))
  (setq orgist--pending-stamps nil)
  (let* ((due-files (orgist--modified-org-files))
         (all-org-files (directory-files orgist-base-dir t "\\`[^.].*\\.org\\'"))
         (all-count (length all-org-files))
         (mod-count (length due-files))
         (skipped (- all-count mod-count))
         (changes '())
         (stamped nil)
         ;; Track which snapshot IDs exist in any buffer, so we
         ;; can detect deletions (IDs in snapshots but no buffer).
         (found-ids (make-hash-table :test 'equal)))
    (when (> skipped 0)
      (orgist-log 'debug "Write-back: skipping %d unmodified file(s), checking %d"
                  skipped mod-count))
    ;; Nothing changed since the last verified scan: no local edits,
    ;; hence no deletions possible either.  Skip the presence pass —
    ;; it touches every snapshot ID in every file, and the auto-pull
    ;; guard runs this scan every few minutes.
    (when (null due-files)
      (cl-return-from orgist-diff-all-elements nil))
    ;; Scan unmodified files to record which snapshot IDs still exist.
    ;; Only build id-cache and check presence — no diffing needed.
    ;; Use find-file-noselect (not find-buffer-visiting) so files
    ;; are opened even if no buffer currently visits them (e.g. after
    ;; subprocess sync creates files the main process hasn't opened).
    (dolist (file all-org-files)
      (unless (assoc file due-files)
        (let ((buf (find-file-noselect file)))
          (with-current-buffer buf
            (unless orgist-id-cache (orgist-build-id-cache))
            (maphash
             (lambda (id _snapshot)
               (condition-case err
                   (when (orgist-find-element-by-id id)
                     (puthash id t found-ids))
                 (error
                  ;; Treat as present rather than risking a spurious
                  ;; item_delete from the deletion pass below.
                  (puthash id t found-ids)
                  (orgist-log 'warn "Presence check failed for %s in %s: %s"
                              id (file-name-nondirectory file)
                              (error-message-string err)))))
             orgist-snapshots)))))
    ;; Diff due files: detect changes and new headings.
    (dolist (entry due-files)
      (let* ((file (car entry))
             (scan-hash (cdr entry))
             (file-changes '())
             (file-errors 0)
             (buf (find-file-noselect file)))
        (with-current-buffer buf
          ;; Rebuild ID cache for modified files so that stale markers
          ;; (from user edits like refile/cut-paste between files) are
          ;; replaced with current positions.
          (orgist-build-id-cache)
          (maphash
           (lambda (id _snapshot)
             (condition-case err
                 (when (orgist-find-element-by-id id)
                   (puthash id t found-ids)
                   (let ((diff (orgist-diff-element id)))
                     (when diff
                       (push (cons id diff) file-changes))))
               (error
                (cl-incf file-errors)
                ;; Conservatively treat the element as present so the
                ;; deletion pass below can't emit an item_delete for an
                ;; element we merely failed to inspect.
                (puthash id t found-ids)
                (orgist-log 'warn "Diff failed for %s in %s: %s (will retry next scan)"
                            id (file-name-nondirectory file)
                            (error-message-string err)))))
           orgist-snapshots)
          ;; Detect new headings.
          ;; - TODO/DONE heading with no :ID: and no SECTION → new task.
          ;; - Level-1 non-TODO heading with no :ID: and no SECTION → new
          ;;   section.  We mark it with :SECTION: t and a placeholder
          ;;   :TODOIST-ORDER: so that subsequent diff/sibling-rank logic
          ;;   treats it like any other section; Todoist assigns the real
          ;;   section_order on creation, which round-trips on next pull.
          (condition-case err
              (save-excursion
                (goto-char (point-min))
                (while (re-search-forward org-heading-regexp nil t)
                  (org-back-to-heading t)
                  (cond
                   ((and (org-get-todo-state)
                         (not (org-entry-get (point) "ID"))
                         (not (org-entry-get (point) "SECTION")))
                    (let ((temp-id (org-id-uuid)))
                      (org-entry-put (point) "ID" temp-id)
                      (orgist-id-cache-put temp-id)
                      (push (cons temp-id 'new) file-changes)
                      (setq file-changes
                            (orgist--merge-sibling-order-changes
                             (orgist--assign-new-heading-order) file-changes))))
                   ((and (= (org-current-level) 1)
                         (not (org-get-todo-state))
                         (not (org-entry-get (point) "ID"))
                         (not (org-entry-get (point) "SECTION")))
                    (let ((temp-id (org-id-uuid)))
                      (org-entry-put (point) "ID" temp-id)
                      (org-entry-put (point) "SECTION" "t")
                      (org-entry-put (point) "TODOIST-ORDER" "0")
                      (orgist-id-cache-put temp-id)
                      (push (cons temp-id 'new-section) file-changes)))
                   ;; Pending section: heading has SECTION=t and an ID, but
                   ;; the ID is not in snapshots — a prior detection marked
                   ;; the heading but the section_add never reached Todoist
                   ;; (e.g. a follow-up save replaced the confirm buffer
                   ;; before the user accepted, or the API call failed).
                   ;; Re-emit so the section actually gets created and any
                   ;; child item_move referencing it can resolve.
                   ((let ((id (org-entry-get (point) "ID")))
                      (and id
                           (org-entry-get (point) "SECTION")
                           (not (gethash id orgist-snapshots))))
                    (push (cons (org-entry-get (point) "ID") 'new-section)
                          file-changes))
                   ;; Pending task: the same failure as the section case
                   ;; above, plus the commoner one — an org-id UUID stamped
                   ;; on the heading (org-store-link, org-capture,
                   ;; org-linker) before the first scan saw it, which
                   ;; permanently disqualifies it from the no-ID branch.
                   ;; Either way it has an ID but no snapshot, so neither
                   ;; the snapshot-keyed diff loop nor the branches above
                   ;; can ever see it.  Only dashed UUIDs qualify: a
                   ;; dash-free ID is a real Todoist ID whose snapshot went
                   ;; missing, and re-adding it would duplicate the task.
                   ((let ((id (org-entry-get (point) "ID")))
                      (and id
                           (string-match-p "-" id)
                           (org-get-todo-state)
                           (not (org-entry-get (point) "SECTION"))
                           (not (gethash id orgist-snapshots))))
                    (push (cons (org-entry-get (point) "ID") 'new)
                          file-changes)
                    (unless (org-entry-get (point) "TODOIST-ORDER")
                      (setq file-changes
                            (orgist--merge-sibling-order-changes
                             (orgist--assign-new-heading-order) file-changes)))))
                  (end-of-line)))
            (error
             (cl-incf file-errors)
             (orgist-log 'warn "New-heading scan failed in %s: %s (will retry next scan)"
                         (file-name-nondirectory file)
                         (error-message-string err)))))
        (setq changes (nconc file-changes changes))
        ;; Stamp bookkeeping.  SCAN-HASH is nil when the visiting
        ;; buffer had unsaved modifications — the on-disk content was
        ;; not what we scanned, so the file cannot be certified and
        ;; stays due.  Files that erred stay due as well.
        (cond
         ((or (null scan-hash) (> file-errors 0)) nil)
         (file-changes
          (push (cons file scan-hash) orgist--pending-stamps))
         (t
          (orgist--stamp-file file scan-hash)
          (setq stamped t)))))
    (when stamped
      (orgist--save-stamps))
    ;; Detect deletions: snapshot IDs not found in any buffer.
    (maphash
     (lambda (id _snapshot)
       (unless (gethash id found-ids)
         (push (cons id 'deleted) changes)))
     orgist-snapshots)
    (nreverse changes)))

(defun orgist-changes-to-commands (changes)
  "Convert CHANGES to Todoist Sync API command objects.
CHANGES is a list of (ELEMENT-ID . DIFF) pairs.
Returns a list of command alists with keys `type', `uuid', `args'."
  (let ((commands '())
        ;; Label cache refreshed at most once per batch (see :labels).
        (labels-refreshed nil))
    (dolist (change changes)
      (let ((id (car change))
            (diff (cdr change)))
        (cond
         ;; Deleted element
         ((eq diff 'deleted)
          (let ((snapshot (gethash id orgist-snapshots)))
            (push (list (cons 'type (if (plist-get snapshot :section-p)
                                        "section_delete"
                                      "item_delete"))
                        (cons 'uuid (org-id-uuid))
                        (cons 'args (list (cons 'id id))))
                  commands)))
         ;; New section — build section_add command.  The temp_id is
         ;; the heading's local UUID (set during diff detection); item_move
         ;; / item_add commands later in the same batch can reference it
         ;; as section_id and Todoist resolves the temp_id to the real
         ;; section ID server-side.
         ((eq diff 'new-section)
          (catch 'built-section
            (dolist (file (directory-files orgist-base-dir t "\\`[^.].*\\.org\\'"))
              (when-let* ((buf (find-buffer-visiting file)))
                (with-current-buffer buf
                  (when-let* ((pos (orgist-find-element-by-id id)))
                    (save-excursion
                      (goto-char pos)
                      (let ((name (car (orgist-extract-heading-and-tags)))
                            (project-id (org-entry-get (point-min) "ID")))
                        (push (list (cons 'type "section_add")
                                    (cons 'uuid (org-id-uuid))
                                    (cons 'temp_id id)
                                    (cons 'args (list (cons 'name name)
                                                      (cons 'project_id project-id))))
                              commands)
                        (throw 'built-section nil)))))))))
         ;; New element — build item_add command
         ((eq diff 'new)
          (catch 'built
            (dolist (file (directory-files orgist-base-dir t "\\`[^.].*\\.org\\'"))
              (let ((buf (find-buffer-visiting file)))
                (when buf
                  (with-current-buffer buf
                    (when-let* ((pos (orgist-find-element-by-id id)))
                      (save-excursion
                        (goto-char pos)
                        (let* ((local (orgist-element-local-state))
                               (project-id (org-entry-get (point-min) "ID"))
                               (parent-id (plist-get local :parent-id))
                               (parent-snap (when parent-id
                                              (gethash parent-id orgist-snapshots)))
                               (add-args
                                (list (cons 'content (orgist-convert-content-to-markdown
                                                      (plist-get local :content)))
                                      (cons 'project_id project-id)))
                               ;; Determine parent placement from what the
                               ;; parent heading actually is: SECTION
                               ;; property → section, TODO keyword → task,
                               ;; no TODO keyword → sub-project (API rejects
                               ;; parent_id pointing to a project).
                               ;; TODOIST-ORDER / snapshot :order presence
                               ;; is not a reliable task-vs-project signal:
                               ;; locally created tasks have neither until
                               ;; the item round-trips through a pull.
                               (place-key (cond
                                           ((or (null parent-id)
                                                (equal parent-id project-id))
                                            nil)
                                           ;; Classify by the parent heading
                                           ((save-excursion
                                              (when-let* ((ppos (orgist-find-element-by-id parent-id)))
                                                (goto-char ppos)
                                                (cond
                                                 ((org-entry-get (point) "SECTION")
                                                  'section_id)
                                                 ((org-get-todo-state)
                                                  'parent_id)
                                                 (t 'project_id)))))
                                           ;; Heading not found — fall back
                                           ;; to the snapshot.
                                           ((and parent-snap
                                                 (plist-get parent-snap :section-p))
                                            'section_id)
                                           ((and parent-snap
                                                 (plist-get parent-snap :order))
                                            'parent_id)
                                           (parent-snap 'project_id)
                                           (t nil))))
                          (if (eq place-key 'project_id)
                              ;; Sub-project: replace the root project_id
                              (setf (alist-get 'project_id add-args) parent-id)
                            (when (and place-key parent-id)
                              (push (cons place-key parent-id) add-args)))
                          (let ((pri (plist-get local :priority)))
                            (when (and pri (> pri 1))
                              (push (cons 'priority pri) add-args)))
                          (when-let* ((labels (plist-get local :labels)))
                            (push (cons 'labels (vconcat (mapcar #'orgist--tag-to-label labels)))
                                  add-args))
                          (when-let* ((due (plist-get local :due)))
                            (push (cons 'due (orgist-org-timestamp-to-todoist-due due))
                                  add-args))
                          (when-let* ((deadline (plist-get local :deadline)))
                            (push (cons 'deadline (orgist-org-timestamp-to-todoist-due deadline))
                                  add-args))
                          (when-let* ((dur (plist-get local :duration)))
                            (push (cons 'duration dur) add-args))
                          (let ((desc (plist-get local :description)))
                            (when (and desc (not (string-empty-p desc)))
                              (push (cons 'description
                                          (orgist-convert-description-to-markdown desc))
                                    add-args)))
                          ;; Place the task where the heading sits among
                          ;; its siblings (assigned by
                          ;; `orgist--assign-new-heading-order' during the
                          ;; scan); Todoist would otherwise append it.
                          (when-let* ((order (plist-get local :order)))
                            (push (cons 'child_order order) add-args))
                          (push (list (cons 'type "item_add")
                                      (cons 'uuid (org-id-uuid))
                                      (cons 'temp_id id)
                                      (cons 'args add-args))
                                commands)
                          ;; If the new task is DONE, also complete it.
                          ;; item_add creates tasks as active; a separate
                          ;; item_complete is needed to mark them done.
                          (when (plist-get local :checked)
                            (push (list (cons 'type "item_complete")
                                        (cons 'uuid (org-id-uuid))
                                        (cons 'args (list (cons 'id id))))
                                  commands))
                          (throw 'built nil))))))))))
         ;; Modified element — build update commands
         (t
          (let ((snapshot (gethash id orgist-snapshots))
                (update-args (list (cons 'id id)))
                (needs-update nil)
                (needs-move nil)
                (needs-complete nil)
                (complete-state nil)
                (needs-date-complete nil))
            ;; Collect field changes.
            (dolist (field-change diff)
              (let ((field (car field-change))
                    (new-val (cddr field-change)))
                (pcase field
                  (:content
                   (let ((md-val (orgist-convert-content-to-markdown new-val)))
                     (if (plist-get snapshot :section-p)
                         (push (cons 'name md-val) update-args)
                       (push (cons 'content md-val) update-args)))
                   (setq needs-update t))
                  (:checked
                   (setq needs-complete t)
                   (setq complete-state new-val))
                  (:priority
                   (push (cons 'priority new-val) update-args)
                   (setq needs-update t))
                  (:labels
                   ;; The label cache can be stale (incremental syncs
                   ;; only return changed labels).  A stale cache both
                   ;; mis-converts org tags back to label names and
                   ;; emits label_add for labels that already exist
                   ;; remotely (API code 54).  Refresh once per batch
                   ;; before converting, so a later code 54 failure
                   ;; means something is genuinely wrong.
                   (when (and (not labels-refreshed)
                              (seq-some
                               (lambda (l) (not (orgist-label-name-to-id l)))
                               new-val))
                     (orgist--refresh-labels-from-api)
                     (setq labels-refreshed t))
                   ;; Convert org tags back to Todoist label names
                   (let ((todoist-labels (mapcar #'orgist--tag-to-label new-val)))
                     (push (cons 'labels (vconcat todoist-labels)) update-args)
                     (setq needs-update t)
                     ;; Check for new labels that don't exist yet
                     (let ((old-labels (or (cadr field-change) '())))
                       (dolist (label new-val)
                         (unless (or (member label old-labels)
                                     (orgist-label-name-to-id label))
                           ;; New label — generate label_add command, but deduplicate
                           ;; across tasks in the same write-back batch to avoid
                           ;; sending the same label_add twice (Todoist returns code 54).
                           (let ((todoist-name (orgist--tag-to-label label)))
                             (unless (cl-some (lambda (cmd)
                                               (and (equal (alist-get 'type cmd) "label_add")
                                                    (equal (alist-get 'name (alist-get 'args cmd))
                                                           todoist-name)))
                                             commands)
                               (push (list (cons 'type "label_add")
                                           (cons 'uuid (org-id-uuid))
                                           (cons 'temp_id (org-id-uuid))
                                           (cons 'args (list (cons 'name todoist-name))))
                                     commands))))))))
                  (:due
                   ;; Use the stored Todoist due.string when the repeater
                   ;; is unchanged — this preserves day-of-week anchors
                   ;; (e.g. "every mon") that org's ++Nw cannot encode.
                   ;; When only the interval changes on an anchored repeater
                   ;; (e.g. ++1w → ++2w with stored "every mon"), rebuild
                   ;; the string preserving the anchor.
                   (let* ((old-val (cadr field-change))
                          (old-rep (orgist--extract-repeater old-val))
                          (new-rep (orgist--extract-repeater new-val))
                          (stored-raw (plist-get snapshot :due-string))
                          (same-prefix
                           (and old-rep new-rep
                                (string-match "^\\([.+]+\\)" old-rep)
                                (let ((p (match-string 1 old-rep)))
                                  (and (string-match "^\\([.+]+\\)" new-rep)
                                       (string= p (match-string 1 new-rep))))))
                          (stored (cond
                                   ;; User hand-edited TODOIST_DUE_STRING in
                                   ;; the same sync — push the new recurrence
                                   ;; string verbatim and let Todoist re-parse
                                   ;; and re-anchor it.  This must win over the
                                   ;; anchor-preservation paths below, which
                                   ;; would otherwise re-apply the stale string.
                                   ((assq :due-string diff)
                                    (cddr (assq :due-string diff)))
                                   ((equal old-rep new-rep) stored-raw)
                                   ((and same-prefix stored-raw)
                                    (orgist--rebuild-anchored-due-string
                                     stored-raw new-rep)))))
                     (push (cons 'due (when new-val
                                        (orgist-org-timestamp-to-todoist-due
                                         new-val stored)))
                           update-args))
                   ;; Include duration when due date changes
                   (let ((dur (orgist-org-timestamp-extract-duration new-val)))
                     (push (cons 'duration dur) update-args))
                   (setq needs-update t))
                  (:due-string
                   ;; The user hand-edited TODOIST_DUE_STRING without
                   ;; touching SCHEDULED.  When SCHEDULED also moved, the
                   ;; :due handler already folds in the new string, so do
                   ;; nothing here.  Otherwise rebuild a due object from the
                   ;; unchanged SCHEDULED timestamp carrying the new
                   ;; recurrence string and let Todoist re-parse/re-anchor.
                   (unless (assq :due diff)
                     (let ((ts (plist-get snapshot :due)))
                       (when ts
                         (push (cons 'due (orgist-org-timestamp-to-todoist-due
                                           ts new-val))
                               update-args)
                         (setq needs-update t)))))
                  (:deadline
                   (push (cons 'deadline (when new-val
                                           (orgist-org-timestamp-to-todoist-due new-val)))
                         update-args)
                   (setq needs-update t))
                  (:duration
                   ;; Duration changed independently of due date
                   (unless (assq 'duration update-args)
                     (push (cons 'duration new-val) update-args)
                     (setq needs-update t)))
                  (:description
                   (push (cons 'description
                               (orgist-convert-description-to-markdown
                                (or new-val "")))
                         update-args)
                   (setq needs-update t))
                  (:parent-id
                   (setq needs-move t))
                  (:order
                   ;; Order changes generate item_reorder/section_reorder
                   ;; commands below; must not reset `needs-update' set
                   ;; by other fields in the same diff.
                   nil)
                  (:last-repeat
                   ;; LAST_REPEAT newly set or updated → one instance of
                   ;; a repeating task was completed.  Generate
                   ;; item_close which handles recurring tasks correctly
                   ;; (advances to next occurrence) without needing a due
                   ;; object.  item_update_date_complete was unreliable
                   ;; when the due.date time didn't match the string time.
                   (when new-val
                     (setq needs-date-complete t)
                     ;; Strip the specific date from the due object so
                     ;; item_close can let Todoist compute the next
                     ;; occurrence.  If the repeater prefix changed
                     ;; (e.g. +1w → .+1w), keep the string/is_recurring
                     ;; fields so item_update sends the new recurrence
                     ;; type before item_close advances from it.
                     ;; Look up the :due diff entry (old/new timestamps).
                     (let* ((due-diff (assq :due diff))
                            (due-old-ts (cadr due-diff))
                            (due-new-ts (cddr due-diff))
                            (old-rep (orgist--extract-repeater due-old-ts))
                            (new-rep (orgist--extract-repeater due-new-ts))
                            (prefix-changed
                             (and old-rep new-rep
                                  (string-match "^\\([.+]+\\)" old-rep)
                                  (let ((p (match-string 1 old-rep)))
                                    (not (and (string-match "^\\([.+]+\\)" new-rep)
                                              (string= p (match-string 1 new-rep)))))))
                            (due-pair (assq 'due update-args)))
                       (if (and due-pair prefix-changed)
                           ;; Keep recurrence string/is_recurring, drop the
                           ;; specific date — item_close lets Todoist compute
                           ;; the next occurrence from the new recurrence type.
                           (setcdr due-pair
                                   (seq-remove (lambda (pair) (eq (car pair) 'date))
                                                 (cdr due-pair)))
                         ;; Normal case: drop due entirely.
                         (setq update-args
                               (seq-remove (lambda (pair) (eq (car pair) 'due))
                                             update-args))))
                     (setq update-args
                           (seq-remove (lambda (pair) (eq (car pair) 'duration))
                                         update-args))))
                  (:archived-p
                   ;; Archive/unarchive is a separate command
                   (when (plist-get snapshot :section-p)
                     (push (list (cons 'type (if new-val "section_archive" "section_unarchive"))
                                 (cons 'uuid (org-id-uuid))
                                 (cons 'args (list (cons 'id id))))
                           commands)))
                  (:section-p nil)
                  (:notes nil)))) ; handled separately below
            ;; If last-repeat triggered date-complete, suppress item_update
            ;; when the only remaining update-arg is the id.
            (when (and needs-date-complete
                      needs-update
                      (length= update-args 1)
                      (assq 'id update-args))
              (setq needs-update nil))
            ;; Emit note_add commands for new logbook Notes.
            (when-let* ((notes-diff (assq :notes diff)))
              (let* ((known-count (cadr notes-diff))
                     ;; Extract all logbook notes and take the new ones.
                     ;; seq-uniq mirrors the diff detection — stale
                     ;; duplicate notes are never pushed.
                     (all-notes
                      (catch 'found-notes
                        (dolist (file (directory-files orgist-base-dir t "\\`[^.].*\\.org\\'"))
                          (when-let* ((buf (find-buffer-visiting file)))
                            (with-current-buffer buf
                              (when-let* ((pos (orgist-find-element-by-id id)))
                                (save-excursion
                                  (goto-char pos)
                                  (throw 'found-notes
                                         (seq-uniq
                                          (orgist-extract-logbook-notes)
                                          #'equal)))))))
                        nil))
                     (new-notes (nthcdr known-count all-notes)))
                (dolist (note new-notes)
                  (let ((note-text (cdr note)))
                    (push (list (cons 'type "note_add")
                                (cons 'uuid (org-id-uuid))
                                (cons 'temp_id (org-id-uuid))
                                (cons 'args (list (cons 'item_id id)
                                                  (cons 'content
                                                        (orgist-convert-description-to-markdown
                                                         note-text)))))
                          commands)))))
            ;; Emit commands.
            (when needs-update
              (push (list (cons 'type (if (plist-get snapshot :section-p)
                                          "section_update"
                                        "item_update"))
                          (cons 'uuid (org-id-uuid))
                          (cons 'args update-args))
                    commands))
            (when needs-date-complete
              ;; Recurring instance completed — use item_close which
              ;; handles recurring tasks like the official clients:
              ;; advances to next occurrence without needing a due object.
              (push (list (cons 'type "item_close")
                          (cons 'uuid (org-id-uuid))
                          (cons 'args (list (cons 'id id))))
                    commands))
            (when needs-complete
              ;; Permanent completion (TODO→DONE) or uncomplete.
              (push (list (cons 'type (if complete-state
                                          "item_complete"
                                        "item_uncomplete"))
                          (cons 'uuid (org-id-uuid))
                          (cons 'args (list (cons 'id id))))
                    commands))
            (when needs-move
              (let* ((move-diff (assq :parent-id diff))
                     (new-parent (cddr move-diff))
                     (parent-snap (when new-parent
                                    (gethash new-parent orgist-snapshots)))
                     ;; Determine the right item_move argument from what
                     ;; the parent heading actually is: SECTION property →
                     ;; section_id, TODO keyword → parent_id (task), no
                     ;; TODO keyword → project_id.  TODOIST-ORDER /
                     ;; snapshot :order presence is not a reliable
                     ;; task-vs-project signal: locally created tasks have
                     ;; neither until the item round-trips through a pull.
                     (move-key (or
                                (catch 'found-type
                                  (dolist (file (directory-files orgist-base-dir t "\\`[^.].*\\.org\\'"))
                                    (when-let* ((buf (find-buffer-visiting file)))
                                      (with-current-buffer buf
                                        (when-let* ((pos (orgist-find-element-by-id new-parent)))
                                          (save-excursion
                                            (goto-char pos)
                                            (throw 'found-type
                                                   (cond
                                                    ;; File-level ID = root project
                                                    ((= pos (point-min)) 'project_id)
                                                    ((org-entry-get (point) "SECTION") 'section_id)
                                                    ((org-get-todo-state) 'parent_id)
                                                    ;; No TODO keyword = sub-project
                                                    (t 'project_id))))))))
                                  nil)
                                ;; Heading not found — fall back to the
                                ;; snapshot, then to a project-level move.
                                (cond
                                 ((and parent-snap
                                       (plist-get parent-snap :section-p))
                                  'section_id)
                                 ((and parent-snap
                                       (plist-get parent-snap :order))
                                  'parent_id)
                                 (t 'project_id)))))
                (push (list (cons 'type "item_move")
                            (cons 'uuid (org-id-uuid))
                            (cons 'args (list (cons 'id id)
                                              (cons move-key new-parent))))
                      commands)))
            ;; Emit reorder command when order changed
            (when (assq :order diff)
              (let* ((order-diff (assq :order diff))
                     (new-order (cddr order-diff))
                     (is-section (plist-get snapshot :section-p)))
                (push (list (cons 'type (if is-section
                                            "section_reorder"
                                          "item_reorder"))
                            (cons 'uuid (org-id-uuid))
                            (cons 'args
                                  (if is-section
                                      (list (cons 'sections
                                                  (vector (list (cons 'id id)
                                                                (cons 'section_order new-order)))))
                                    (list (cons 'items
                                                (vector (list (cons 'id id)
                                                              (cons 'child_order new-order))))))))
                      commands)))
            ;; Emit attachment commands when files changed.
            ;; Use multiset (consume-one) comparison so duplicate filenames
            ;; in old-files or new-files are each matched at most once.
            (when-let* ((attach-diff (assq :attachment-files diff)))
              (let ((old-files (or (cadr attach-diff) '()))
                    (new-files (or (cddr attach-diff) '())))
                ;; New local files → upload those not covered by old-files.
                (let ((old-remaining (copy-sequence old-files)))
                  (dolist (file-name new-files)
                    (let ((pos (cl-position file-name old-remaining :test #'equal)))
                      (if pos
                          (setq old-remaining
                                (append (cl-subseq old-remaining 0 pos)
                                        (cl-subseq old-remaining (1+ pos))))
                        (push (list (cons 'type "attachment_upload")
                                    (cons 'uuid (org-id-uuid))
                                    (cons 'args (list (cons 'item_id id)
                                                      (cons 'file_name file-name))))
                              commands)))))
                ;; Deleted local files → delete comments for old-files not covered
                ;; by new-files.  Snapshot alist is (comment-id . filename).
                (let ((new-remaining (copy-sequence new-files))
                      (snap-remaining (copy-sequence
                                       (plist-get snapshot :attachment-files))))
                  (dolist (file-name old-files)
                    (let ((pos (cl-position file-name new-remaining :test #'equal)))
                      (if pos
                          (setq new-remaining
                                (append (cl-subseq new-remaining 0 pos)
                                        (cl-subseq new-remaining (1+ pos))))
                        ;; Prefer consuming nil-keyed ghost entries first.
                        ;; Only generate a delete command when exactly one real
                        ;; comment-id maps to this filename.  Multiple real
                        ;; entries means the snapshot is ambiguous (two distinct
                        ;; Todoist comments share a local filename); consuming
                        ;; silently avoids deleting a legitimate comment.
                        (let* ((nil-entry (seq-find
                                           (lambda (e)
                                             (and (null (car e))
                                                  (equal (cdr e) file-name)))
                                           snap-remaining))
                               (real-entries (unless nil-entry
                                               (seq-filter
                                                (lambda (e)
                                                  (and (car e)
                                                       (equal (cdr e) file-name)))
                                                snap-remaining)))
                               ;; delete-entry is only set when unambiguous
                               (delete-entry (when (= (length real-entries) 1)
                                               (car real-entries)))
                               (consume-entry (or nil-entry delete-entry
                                                  (car real-entries))))
                          (when consume-entry
                            (setq snap-remaining (delq consume-entry snap-remaining))
                            (when delete-entry
                              (push (list (cons 'type "attachment_delete")
                                          (cons 'uuid (org-id-uuid))
                                          (cons 'args (list (cons 'comment_id (car delete-entry))
                                                            (cons 'file_name file-name))))
                                    commands)))))))
                  ;; Persist the cleaned alist so consumed nil-ghost and
                  ;; ambiguous entries don't trigger a spurious diff on the
                  ;; next write-back cycle.
                  (plist-put snapshot :attachment-files snap-remaining)
                  (puthash id snapshot orgist-snapshots)))))))))
    ;; Stable-sort so that section_add commands precede any command
    ;; that may reference their temp_id (item_move / item_add with
    ;; section_id, etc.).  Todoist resolves temp_ids in the order
    ;; commands appear in the batch.
    (let ((ordered (nreverse commands))
          (sections '())
          (rest '()))
      (dolist (cmd ordered)
        (if (equal (alist-get 'type cmd) "section_add")
            (push cmd sections)
          (push cmd rest)))
      (append (nreverse sections) (nreverse rest)))))

(defun orgist-group-changes-by-project (changes)
  "Group CHANGES by project file.
Returns alist of (PROJECT-NAME . ITEMS) where each ITEM is
  (ELEMENT-ID TASK-NAME DIFF-TYPE DIFF).
DIFF-TYPE is one of `modified', `deleted', or `new'."
  (let ((project-map (make-hash-table :test 'equal)))
    (dolist (change changes)
      (let* ((id (car change))
             (diff (cdr change))
             (snapshot (gethash id orgist-snapshots))
             (task-name (or (when snapshot (plist-get snapshot :content))
                           ;; New tasks have no snapshot yet — read from buffer
                           (catch 'name
                             (dolist (file (directory-files orgist-base-dir t "\\`[^.].*\\.org\\'"))
                               (when-let* ((buf (find-buffer-visiting file)))
                                 (with-current-buffer buf
                                   (when-let* ((pos (orgist-find-element-by-id id)))
                                     (save-excursion
                                       (goto-char pos)
                                       (throw 'name
                                              (car (orgist-extract-heading-and-tags))))))))
                             "?")))
             (diff-type (cond ((eq diff 'deleted) 'deleted)
                              ;; Both 'new (task) and 'new-section
                              ;; render the same way in the confirm UI.
                              ((memq diff '(new new-section)) 'new)
                              (t 'modified)))
             (project-name nil))
        ;; Find which project file this element belongs to
        (catch 'found
          (dolist (file (directory-files orgist-base-dir t "\\`[^.].*\\.org\\'"))
            (let ((buf (find-buffer-visiting file)))
              (when buf
                (with-current-buffer buf
                  (when (orgist-find-element-by-id id)
                    (setq project-name (file-name-base file))
                    (throw 'found nil)))))))
        ;; Deleted tasks won't be in any buffer — resolve project
        ;; by walking the snapshot's :parent-id up to a file-level ID.
        (when (and (not project-name) snapshot)
          (let ((parent (plist-get snapshot :parent-id)))
            (catch 'found
              (while parent
                (dolist (file (directory-files orgist-base-dir t "\\`[^.].*\\.org\\'"))
                  (when-let* ((buf (find-buffer-visiting file)))
                    (with-current-buffer buf
                      (when-let* ((pos (orgist-find-element-by-id parent)))
                        (when (= pos (point-min))
                          (setq project-name (file-name-base file))
                          (throw 'found nil))))))
                ;; Walk up: check if this parent has its own snapshot
                (let ((parent-snap (gethash parent orgist-snapshots)))
                  (setq parent (when parent-snap
                                 (plist-get parent-snap :parent-id))))))))
        (unless project-name
          (setq project-name "(unknown)"))
        (push (list id task-name diff-type diff)
              (gethash project-name project-map nil))))
    ;; Convert hash to alist, reverse items to preserve order
    (let ((result '()))
      (maphash (lambda (project items)
                 (push (cons project (nreverse items)) result))
               project-map)
      (sort result (lambda (a b) (string< (car a) (car b)))))))

(defun orgist--refresh-recurring-dates (item-ids)
  "Fetch Todoist's computed next dates for ITEM-IDS and apply them.
After `item_close', Todoist advances the recurring date according
to its own recurrence rules, which may differ from
org-mode's local computation.  This function fetches each task via
the REST API and overwrites the org heading's schedule, due
string, TODO state, and LAST_REPEAT so the buffer reflects
Todoist's authoritative next occurrence."
  (dolist (id item-ids)
    (let ((task nil))
      (orgist--request-with-retry
        (format "https://api.todoist.com/api/v1/tasks/%s" id)
        :headers `(("Authorization" . ,(format "Bearer %s" orgist-bearer-token)))
        :parser 'json-read
        :sync t
        :error (cl-function
                (lambda (&key data error-thrown &allow-other-keys)
                  (let ((code (orgist--http-error-code error-thrown)))
                    (if (memq code '(403 404 410))
                        (orgist-log 'debug "Refresh date: task %s returned HTTP %s, skipping"
                                    id code)
                      (orgist-log 'warn "Refresh date: API error for %s: %S (err=%S)"
                                  id data error-thrown)))))
        :success (cl-function
                  (lambda (&key data &allow-other-keys)
                    (setq task data))))
      (when task
        (let ((due (alist-get 'due task))
              (duration (alist-get 'duration task)))
          (catch 'done
            (dolist (file (directory-files orgist-base-dir t "\\`[^.].*\\.org\\'"))
              (let ((buf (find-buffer-visiting file)))
                (when buf
                  (with-current-buffer buf
                    (when-let* ((pos (orgist-find-element-by-id id)))
                      ;; Keep `org-schedule'/`org-todo' from looping over an
                      ;; active user region.
                      (let ((org-loop-over-headlines-in-active-region nil))
                        (save-excursion
                          (goto-char pos)
                          ;; Apply Todoist's authoritative next date
                          (if due
                              (let ((org-ts (orgist-parse-todoist-date-with-duration due duration))
                                    (due-string (alist-get 'string due)))
                                (when org-ts
                                  (org-schedule nil org-ts))
                                (if due-string
                                    (org-entry-put (point) "TODOIST_DUE_STRING" due-string)
                                  (org-entry-delete (point) "TODOIST_DUE_STRING")))
                            ;; No due date returned — clear schedule
                            (org-schedule '(4))
                            (org-entry-delete (point) "TODOIST_DUE_STRING"))
                          ;; Reset TODO state (org set it to DONE on completion)
                          (let ((org-inhibit-logging t))
                            (org-todo "TODO"))
                          ;; Clear LAST_REPEAT so write-back doesn't re-detect
                          (org-entry-delete (point) "LAST_REPEAT")
                          (orgist-log 'debug "Refreshed recurring date for %s from Todoist"
                                      id)))
                      (throw 'done nil))))))))))))

(defun orgist-execute-write-back (commands)
  "Execute COMMANDS: dry-run log or send to API.
After execution, update snapshots so the same changes are not
detected again on the next write-back cycle."
  ;; Split attachment commands (REST API) from sync commands.
  (let ((sync-commands '())
        (attach-commands '())
        (orgist--last-temp-id-mapping nil))
    (dolist (cmd commands)
      (if (member (alist-get 'type cmd) '("attachment_upload" "attachment_delete"))
          (push cmd attach-commands)
        (push cmd sync-commands)))
    (setq sync-commands (nreverse sync-commands))
    (setq attach-commands (nreverse attach-commands))
    (if orgist-write-back-dry-run
        (progn
          (orgist-log-commands commands "[DRY-RUN]")
          (orgist-update-snapshots-from-local commands)
          (orgist-save-snapshots)
          ;; Snapshots advanced, so these changes won't re-detect;
          ;; stamp the scanned files as verified.
          (orgist--commit-pending-stamps))
      ;; Execute attachment commands first (REST API)
      (when attach-commands
        (orgist-log-commands attach-commands "[ATTACH]")
        (orgist-execute-attachment-commands attach-commands))
      ;; Execute sync commands.  Only commands confirmed "ok" in the
      ;; API response are used to advance snapshots, so a failed
      ;; request can't leave behind a temp-id snapshot that later
      ;; surfaces as a phantom deletion.
      (let ((succeeded-sync-commands '()))
        (when sync-commands
          (orgist-log-commands sync-commands "[SEND]")
          (condition-case err
              (let* ((result (orgist-send-commands sync-commands))
                     (sync-status (plist-get result :sync-status))
                     (temp-id-mapping (plist-get result :temp-id-mapping)))
                (orgist-process-write-back-results sync-commands sync-status)
                (when temp-id-mapping
                  (setq orgist--last-temp-id-mapping temp-id-mapping)
                  (orgist-remap-temp-ids sync-commands temp-id-mapping))
                (setq succeeded-sync-commands
                      (seq-filter
                       (lambda (cmd)
                         (equal (alist-get (intern (alist-get 'uuid cmd))
                                           sync-status)
                                "ok"))
                       sync-commands))
                ;; Refresh recurring dates from Todoist for completed instances.
                ;; item_close lets Todoist compute the real next date;
                ;; fetch it back so the org heading is authoritative.
                (let ((recurring-ids
                       (cl-loop for cmd in succeeded-sync-commands
                                when (equal (alist-get 'type cmd) "item_close")
                                collect (alist-get 'id (alist-get 'args cmd)))))
                  (when recurring-ids
                    (orgist-log 'debug "Refreshing %d recurring date(s) from Todoist"
                                (length recurring-ids))
                    (orgist--refresh-recurring-dates recurring-ids))))
            (error
             ;; API call failed entirely — show error buffer so the user
             ;; sees what happened (the error message would otherwise
             ;; flash briefly in the minibuffer and disappear).
             ;; succeeded-sync-commands stays empty so snapshots are
             ;; NOT advanced for any of these commands.
             (orgist--show-error-buffer
              0 (length sync-commands)
              (list (format "  API call failed\n    %s"
                            (error-message-string err)))))))
        ;; Advance snapshots only for commands that actually went through:
        ;; attachment commands (handled above, idempotent here) plus
        ;; sync commands whose sync_status came back "ok".
        (let ((applied (append attach-commands succeeded-sync-commands)))
          (orgist-update-snapshots-from-local applied orgist--last-temp-id-mapping)
          ;; Sync metadata comments (non-Todoist properties).
          ;; Pass temp-id-mapping so new tasks (with temp IDs in commands
          ;; but real IDs in the buffer after remap) can be found.
          (orgist-sync-metadata-comments applied orgist--last-temp-id-mapping))
        ;; Persist any buffer modifications made during this write-back
        ;; (temp-id → real-id remap, recurring date refresh) so the user
        ;; doesn't end up with unsaved buffers.  Inhibit `after-save' to
        ;; avoid re-triggering the orgist write-back cycle on these saves.
        (let ((orgist--inhibit-after-save t))
          (dolist (file (directory-files orgist-base-dir t "\\`[^.].*\\.org\\'"))
            (when-let* ((buf (find-buffer-visiting file)))
              (with-current-buffer buf
                (when (and orgist-mode (buffer-modified-p))
                  (save-buffer))))))
        (orgist-save-snapshots)
        ;; Advance write-back stamps only when every sync command was
        ;; confirmed "ok".  On any failure the scanned files keep their
        ;; old stamps, so the next save or sync re-detects and retries
        ;; the remaining changes (succeeded commands advanced their
        ;; snapshots above and won't re-emit).  Note: files whose
        ;; buffers were edited during execution (temp-id remap, date
        ;; refresh) were stamped with their scan-time hash, so they
        ;; stay due and the follow-up scan re-verifies them clean.
        (if (= (length succeeded-sync-commands) (length sync-commands))
            (orgist--commit-pending-stamps)
          (setq orgist--pending-stamps nil)
          (orgist-log 'debug
                      "Write-back stamps not advanced: %d of %d command(s) failed"
                      (- (length sync-commands) (length succeeded-sync-commands))
                      (length sync-commands)))))))

(defun orgist-execute-attachment-commands (commands)
  "Execute attachment upload/delete COMMANDS via REST API.
Updates snapshots with the new attachment-files entries."
  (dolist (cmd commands)
    (let* ((cmd-type (alist-get 'type cmd))
           (args (alist-get 'args cmd)))
      (condition-case err
          (pcase cmd-type
            ("attachment_upload"
             (let* ((item-id (alist-get 'item_id args))
                    (file-name (alist-get 'file_name args))
                    (file-path
                     (catch 'found
                       (dolist (file (directory-files orgist-base-dir t "\\`[^.].*\\.org\\'"))
                         (when-let* ((buf (find-buffer-visiting file)))
                           (with-current-buffer buf
                             (when-let* ((pos (orgist-find-element-by-id item-id)))
                               (save-excursion
                                 (goto-char pos)
                                 ;; Check this heading's attach dir
                                 (when-let* ((dir (org-attach-dir)))
                                   (let ((fp (expand-file-name file-name dir)))
                                     (when (file-exists-p fp)
                                       (throw 'found fp))))
                                 ;; Check non-TODO child headings
                                 (let ((subtree-end (orgist--subtree-end))
                                       (level (org-current-level)))
                                   (while (and (outline-next-heading)
                                               (< (point) subtree-end))
                                     (when (and (= (org-current-level) (1+ level))
                                                (not (org-get-todo-state)))
                                       (when-let* ((dir (org-attach-dir)))
                                         (let ((fp (expand-file-name file-name dir)))
                                           (when (file-exists-p fp)
                                             (throw 'found fp))))))))))))
                       nil)))
               (if (not file-path)
                   (orgist-log 'warn "Attachment file not found: %s" file-name)
                 (orgist-log 'debug "Uploading attachment %s for task %s"
                             file-name item-id)
                 (let ((meta (orgist-upload-file file-path)))
                   (if (not meta)
                       (orgist-log 'warn "Upload failed for %s" file-name)
                     (let ((comment (orgist-create-comment-with-attachment
                                     item-id meta)))
                       (if (not comment)
                           (orgist-log 'warn "Comment creation failed for %s" file-name)
                         ;; Update snapshot with new attachment tracking
                         (let* ((comment-id (alist-get 'id comment))
                                (snap (gethash item-id orgist-snapshots)))
                           (when snap
                             (let ((attachments (plist-get snap :attachment-files)))
                               (push (cons comment-id file-name) attachments)
                               (plist-put snap :attachment-files attachments)
                               (puthash item-id snap orgist-snapshots)))
                           (orgist-log 'debug "Uploaded attachment %s (comment %s)"
                                       file-name comment-id)))))))))
            ("attachment_delete"
             (let* ((comment-id (alist-get 'comment_id args))
                    (file-name (alist-get 'file_name args)))
               (orgist-log 'debug "Deleting attachment comment %s (%s)"
                           comment-id file-name)
               (when (orgist-delete-comment comment-id)
                 ;; Remove from snapshot
                 (maphash
                  (lambda (_id snap)
                    (let ((attachments (plist-get snap :attachment-files)))
                      (when (assoc comment-id attachments)
                        (plist-put snap :attachment-files
                                   (assoc-delete-all comment-id attachments)))))
                  orgist-snapshots)))))
        (error
         (orgist-log 'warn "Attachment command failed: %s" (error-message-string err)))))))

(defun orgist-remap-temp-ids (commands temp-id-mapping)
  "Remap temporary IDs to real Todoist IDs after successful item_add.
COMMANDS is the list of commands sent.  TEMP-ID-MAPPING is an alist
mapping temp UUIDs to real Todoist IDs from the API response."
  (dolist (cmd commands)
    (when (member (alist-get 'type cmd) '("item_add" "section_add"))
      (let* ((temp-id (alist-get 'temp_id cmd))
             (real-id (alist-get (intern temp-id) temp-id-mapping)))
        (when real-id
          (orgist-log 'debug "Remapping temp ID %s -> %s" temp-id real-id)
          ;; Update heading :ID: property, id-cache, and snapshots
          (catch 'done
            (dolist (file (directory-files orgist-base-dir t "\\`[^.].*\\.org\\'"))
              (when-let* ((buf (find-buffer-visiting file)))
                (with-current-buffer buf
                  (when-let* ((pos (orgist-find-element-by-id temp-id)))
                    (save-excursion
                      (goto-char pos)
                      (org-entry-put (point) "ID" real-id)
                      ;; Re-key id-cache while point is at the heading
                      ;; so orgist-id-cache-put records the correct position.
                      (remhash temp-id orgist-id-cache)
                      (orgist-id-cache-put real-id))
                    ;; Re-key snapshots
                    (when-let* ((snap (gethash temp-id orgist-snapshots)))
                      (remhash temp-id orgist-snapshots)
                      (puthash real-id snap orgist-snapshots))
                    ;; Update the command's temp_id so snapshot-update finds it
                    (setcdr (assq 'temp_id cmd) real-id)
                    (throw 'done nil)))))))))))

(defun orgist--command-snapshot-keys (cmd-type args)
  "Return the snapshot keys that CMD-TYPE with ARGS actually synced.
Only these keys may be advanced to local state after the command
succeeds — advancing anything more would record unsent local edits
as synced and create silent, undetectable drift (the snapshot is a
mirror of the remote, not of local intent).
Returns the symbol `all' for commands that create the element
remotely (item_add / section_add), where the full local state is
the correct baseline."
  (pcase cmd-type
    ((or "item_update" "section_update")
     (let (keys)
       (dolist (pair args)
         (pcase (car pair)
           ((or 'content 'name) (push :content keys))
           ('priority (push :priority keys))
           ('labels (push :labels keys))
           ;; A due change also refreshes TODOIST_DUE_STRING in the
           ;; buffer before this merge runs, so both keys are synced.
           ('due (setq keys (append '(:due :due-string) keys)))
           ('deadline (push :deadline keys))
           ('duration (push :duration keys))
           ('description (push :description keys))))
       keys))
    ("item_move" '(:parent-id))
    ((or "item_reorder" "section_reorder") '(:order))
    ((or "item_complete" "item_uncomplete") '(:checked))
    ;; item_close completes a recurring instance; the buffer date is
    ;; refreshed from Todoist before this merge runs.
    ("item_close" '(:checked :due :due-string :duration :last-repeat))
    ((or "section_archive" "section_unarchive") '(:archived-p))
    ((or "item_add" "section_add") 'all)
    (_ nil)))

(defun orgist-update-snapshots-from-local (commands &optional temp-id-mapping)
  "Update `orgist-snapshots' to match current local state for each
element referenced in COMMANDS.  This prevents the same changes
from being detected on the next diff cycle.
TEMP-ID-MAPPING, when non-nil, is the alist from the Sync API mapping
temp UUIDs to real Todoist IDs; used to store the real comment ID
for note_add commands instead of a content fingerprint."
  (dolist (cmd commands)
    (let* ((cmd-type (alist-get 'type cmd))
           (args (alist-get 'args cmd))
           (id (or (alist-get 'id args)
                   ;; item_add has no id in args — use temp_id from cmd
                   (alist-get 'temp_id cmd)
                   ;; item_reorder/section_reorder: ID nested in items[0]/sections[0]
                   (when-let* ((vec (or (alist-get 'items args)
                                        (alist-get 'sections args))))
                     (alist-get 'id (aref vec 0))))))
      (when id
        (cond
         ;; Deletion — remove snapshot so it won't be re-detected
         ((member cmd-type '("item_delete" "section_delete"))
          (remhash id orgist-snapshots))
         ;; note_add — record the comment ID in the task's :comment-ids.
         ;; Prefer the real Todoist comment ID from temp_id_mapping so the
         ;; comment pull can correctly identify this as already-known.
         ;; Fall back to a content fingerprint when the mapping is absent.
         ((string= cmd-type "note_add")
          (let* ((item-id (alist-get 'item_id args))
                 (snap (when item-id (gethash item-id orgist-snapshots))))
            (when snap
              (let* ((updated (copy-sequence snap))
                     (note-content (alist-get 'content args))
                     (cmd-temp-id (alist-get 'temp_id cmd))
                     (real-id (when (and temp-id-mapping cmd-temp-id)
                                (alist-get (intern cmd-temp-id) temp-id-mapping)))
                     (stored-id (or real-id
                                    (orgist-note-fingerprint "" note-content)))
                     (ids (or (plist-get updated :comment-ids) '())))
                (plist-put updated :comment-ids
                           (cons stored-id ids))
                (puthash item-id updated orgist-snapshots)))))
         ;; label_add — record the created label in `orgist-labels' so
         ;; later batches and tag→label conversion know it.  The real
         ;; ID comes from temp_id_mapping; in dry-run there is none and
         ;; nothing was created, so skip.
         ((string= cmd-type "label_add")
          (let* ((cmd-temp-id (alist-get 'temp_id cmd))
                 ;; Real API: alist with symbol keys; test mock: hash table.
                 (real-id (when (and temp-id-mapping cmd-temp-id)
                            (if (hash-table-p temp-id-mapping)
                                (gethash cmd-temp-id temp-id-mapping)
                              (alist-get (intern cmd-temp-id) temp-id-mapping))))
                 (name (alist-get 'name args)))
            (when (and real-id name)
              (unless orgist-labels
                (setq orgist-labels (make-hash-table :test 'equal)))
              (puthash real-id
                       (list (cons 'id real-id) (cons 'name name))
                       orgist-labels)
              (orgist-save-labels))))
         ;; Attachment commands — snapshot already handled by
         ;; orgist-execute-attachment-commands.
         ((member cmd-type '("attachment_upload" "attachment_delete"))
          nil)
         ;; Update/add/move — sync snapshot to current local state
         (t
          (catch 'found
            (dolist (file (directory-files orgist-base-dir t "\\`[^.].*\\.org\\'"))
              (let ((buf (find-buffer-visiting file)))
                (when buf
                  (with-current-buffer buf
                    (when-let* ((pos (orgist-find-element-by-id id)))
                      (save-excursion
                        (goto-char pos)
                        ;; The command may carry a freshly synthesized
                        ;; `due.string' (e.g. switching `+3d' to `.+3d'
                        ;; emits "every! 3 days").  Reflect that in the
                        ;; buffer's TODOIST_DUE_STRING so the property
                        ;; matches what Todoist now has, without waiting
                        ;; for the next pull.  An item_update with a
                        ;; non-nil `due' field but no `string' means the
                        ;; task became non-recurring — drop the property.
                        (when (assq 'due args)
                          (let* ((due (alist-get 'due args))
                                 (new-string (and due (alist-get 'string due))))
                            (cond
                             (new-string
                              (org-entry-put (point) "TODOIST_DUE_STRING"
                                             new-string))
                             ((null due)
                              (org-entry-delete (point) "TODOIST_DUE_STRING")))))
                        (let ((local (orgist-element-local-state))
                              (old (gethash id orgist-snapshots))
                              (keys (orgist--command-snapshot-keys
                                     cmd-type args)))
                          (if (and old (not (eq keys 'all)))
                              ;; Existing snapshot — advance ONLY the
                              ;; fields this command carried.  Local
                              ;; edits that were not sent must keep
                              ;; diffing on the next cycle.  Preserves
                              ;; :comment-ids / :activity-ids (only set
                              ;; by comment/activity pull).
                              (let ((updated (copy-sequence old)))
                                (dolist (key keys)
                                  (plist-put updated key (plist-get local key)))
                                (puthash id updated orgist-snapshots))
                            ;; New element — full local state is the baseline
                            (puthash id local orgist-snapshots))
                          ))
                      (throw 'found nil))))))))))))
  (orgist-log 'debug "Updated snapshots from local state for %d command(s)"
              (length commands)))

(defvar orgist-write-back-timeout 30
  "Timeout in seconds for write-back API requests.
The Todoist Sync API can take several seconds for batch commands.
Increase this if write-back times out on large batches.")

(defcustom orgist-write-back-batch-size 100
  "Maximum number of commands sent in a single Sync API request.
Todoist rejects sync requests with more than 100 commands; larger
write-backs are automatically split into sequential chunks."
  :group 'orgist
  :type 'integer)

(defun orgist--remap-command-args (args mapping)
  "Recursively replace temp-id references in ARGS using MAPPING.
MAPPING is an alist of (SYMBOL . REAL-ID) as returned by the Sync
API in `temp_id_mapping'.  Any string value in ARGS whose interned
symbol is a key in MAPPING is replaced with the real ID."
  (cond
   ((null args) nil)
   ((vectorp args)
    (vconcat (mapcar (lambda (x) (orgist--remap-command-args x mapping)) args)))
   ((and (consp args) (consp (car args)))
    ;; alist — recurse into values, preserve keys
    (mapcar (lambda (pair)
              (cons (car pair)
                    (orgist--remap-command-args (cdr pair) mapping)))
            args))
   ((stringp args)
    (or (alist-get (intern args) mapping nil nil #'eq) args))
   (t args)))

(defun orgist--send-command-chunk (commands)
  "Send a single chunk of COMMANDS to the Todoist Sync API.
Returns a plist (:sync-status ALIST :temp-id-mapping ALIST).

The response also contains a sync token, but it is deliberately ignored:
a commands-only request does not return resource changes.  Persisting its
token would advance the read cursor past remote changes that Orgist has not
downloaded or applied."
  (let* ((json-commands (json-encode (vconcat commands)))
         (response nil))
    (orgist--request-with-retry
      "https://api.todoist.com/api/v1/sync"
      :headers `(("Authorization" . ,(format "Bearer %s" orgist-bearer-token))
                 ("Connection" . "close"))
      :data `(("commands" . ,json-commands))
      :parser 'json-read
      :sync t
      :timeout orgist-write-back-timeout
      :error (cl-function
              (lambda (&key data error-thrown symbol-status response
                       &allow-other-keys)
                (let ((status-code (when response
                                     (request-response-status-code response)))
                      (detail (or (alist-get 'error data)
                                  (and (consp error-thrown) (cdr error-thrown))
                                  error-thrown)))
                  (orgist-log 'warn "Write-back API error: HTTP %s %s — %S"
                              (or status-code "?") (or symbol-status "?") detail)
                  (error "Write-back API call failed: HTTP %s — %S"
                         (or status-code "?") detail))))
      :success (cl-function
                (lambda (&key data &allow-other-keys)
                  (setq response data))))
    (unless response
      (orgist-log 'warn "Write-back: API returned empty response"))
    (list :sync-status (alist-get 'sync_status response)
          :temp-id-mapping (alist-get 'temp_id_mapping response))))

(defun orgist-send-commands (commands)
  "Send COMMANDS to the Todoist Sync API.
COMMANDS is a list of command alists (type, uuid, args).  If the
list exceeds `orgist-write-back-batch-size' it is split into
chunks sent sequentially; temp-id references produced by earlier
chunks are rewritten to real IDs in later chunks.
Returns a plist (:sync-status ALIST :temp-id-mapping ALIST)
merging results across all chunks."
  (let* ((batch-size (max 1 orgist-write-back-batch-size))
         (total (length commands))
         (total-chunks (max 1 (ceiling (/ (float total) batch-size))))
         (merged-status '())
         (merged-mapping '())
         (remaining commands)
         (chunk-num 0))
    (orgist-log 'info "Sending %d command(s) to Todoist API in %d chunk(s)"
                total total-chunks)
    (while remaining
      (setq chunk-num (1+ chunk-num))
      (let* ((take (min batch-size (length remaining)))
             (chunk (cl-subseq remaining 0 take))
             (rest (nthcdr take remaining)))
        ;; Rewrite temp-id references in this chunk using real IDs from
        ;; temp_id_mapping returned by previous chunks.
        (when merged-mapping
          (dolist (cmd chunk)
            (when-let* ((args-pair (assq 'args cmd))
                        (args (cdr args-pair)))
              (setcdr args-pair
                      (orgist--remap-command-args args merged-mapping)))))
        (when (> total-chunks 1)
          (orgist-log 'info "Sending chunk %d/%d (%d command(s))"
                      chunk-num total-chunks take))
        (let ((result (orgist--send-command-chunk chunk)))
          (setq merged-status
                (append merged-status (plist-get result :sync-status)))
          (when-let* ((m (plist-get result :temp-id-mapping)))
            (setq merged-mapping (append merged-mapping m))))
        (setq remaining rest)))
    (orgist-log 'debug "Write-back sync_status: %S" merged-status)
    (when merged-mapping
      (orgist-log 'debug "Write-back temp_id_mapping: %S" merged-mapping))
    (list :sync-status merged-status :temp-id-mapping merged-mapping)))

(defun orgist-process-write-back-results (commands sync-status)
  "Log results of write-back COMMANDS using SYNC-STATUS.
Shows a *Orgist Errors* buffer when any command fails.
Returns the number of failed commands."
  (let ((failed 0)
        (succeeded 0)
        (failure-lines '()))
    (dolist (cmd commands)
      (let* ((uuid (alist-get 'uuid cmd))
             (cmd-type (alist-get 'type cmd))
             (args (alist-get 'args cmd))
             (id (alist-get 'id args))
             (snapshot (when id (gethash id orgist-snapshots)))
             (snap-name (when snapshot (plist-get snapshot :content)))
             (label (orgist--id-label id snap-name))
             (status (alist-get (intern uuid) sync-status)))
        (if (equal status "ok")
            (progn
              (setq succeeded (1+ succeeded))
              (orgist-log 'debug "Command OK: %s %s" cmd-type label))
          (setq failed (1+ failed))
          (let ((reason (cond
                         ((null status) "(no status returned by API)")
                         ((and (listp status) (alist-get 'error status))
                          (format "%s (code %s)"
                                  (alist-get 'error status)
                                  (alist-get 'error_code status)))
                         (t (format "%S" status)))))
            (orgist-log 'warn "Command FAILED: %s %s — %s" cmd-type label reason)
            (push (format "  %s %s\n    %s" cmd-type label reason)
                  failure-lines)))))
    (orgist-log 'info "Write-back results: %d succeeded, %d failed"
                succeeded failed)
    (when (> failed 0)
      (orgist--show-error-buffer succeeded failed (nreverse failure-lines)))
    failed))

(defun orgist--show-error-buffer (succeeded failed failure-lines)
  "Display *Orgist Errors* buffer with FAILURE-LINES.
SUCCEEDED and FAILED are the respective command counts."
  (let ((buf (get-buffer-create "*Orgist Errors*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "Orgist write-back: %d succeeded, %d FAILED\n\n"
                        succeeded failed))
        (insert "Failed commands:\n\n")
        (dolist (line failure-lines)
          (insert line "\n\n"))
        (insert "\nThese changes were NOT applied to Todoist.\n")
        (insert "Fix the issue and re-sync, or revert the local change.\n"))
      (goto-char (point-min))
      (special-mode))
    (display-buffer buf)))

(defun orgist-log-commands (commands tag)
  "Log COMMANDS with TAG prefix (e.g. \"[DRY-RUN]\" or \"[SEND]\")."
  (orgist-log 'info "%s %d command(s):" tag (length commands))
  (cl-loop for cmd in commands
           for i from 1
           for cmd-type = (alist-get 'type cmd)
           for args = (alist-get 'args cmd)
           for id = (alist-get 'id args)
           for snapshot = (when id (gethash id orgist-snapshots))
           for snap-name = (when snapshot (plist-get snapshot :content))
           for detail = (cl-loop for (k . v) in args
                                 unless (eq k 'id)
                                 collect (format "%s=%S" k v))
           do (orgist-log 'info "%s   %d. %s %s %s"
                          tag i cmd-type (orgist--id-label id snap-name)
                          (string-join detail " "))))

(defun orgist-write-back (&optional continuation)
  "Detect local changes and write them back to Todoist.
Respects `orgist-enable-write-back' and `orgist-write-back-dry-run'.
When `orgist-enable-write-back' is `ask', shows a confirmation buffer
and returns `pending' — execution continues asynchronously via callback.
CONTINUATION, if non-nil, is a function called after the user confirms
in the confirmation buffer.  It is only used in `ask' mode; cancelling
aborts the entire sync cycle.  In other modes the function returns
synchronously and the caller is responsible for continuing."
  (interactive)
  (cond
   ((not orgist-enable-write-back)
    (orgist-log 'debug "Write-back disabled, skipping"))
   (t
    (orgist-load-snapshots)
    (if (= (hash-table-count orgist-snapshots) 0)
        (orgist-log 'debug "No snapshots found (first sync?), skipping write-back")
      (let ((changes (orgist-diff-all-elements)))
        (if (not changes)
            (orgist-log 'debug "No local changes detected")
          (orgist-log 'debug "Detected %d local change(s)" (length changes))
          (let ((commands (orgist-changes-to-commands changes)))
            (if (not commands)
                ;; Changes that generate no commands (suppressed
                ;; fields, already-known notes) would re-detect on
                ;; every scan; certify the scanned files so they
                ;; don't stay due forever.
                (orgist--commit-pending-stamps)
              (if (or (eq orgist-enable-write-back t) noninteractive)
                  (orgist-execute-write-back commands)
                ;; 'ask mode — show confirmation buffer, return 'pending
                (orgist-confirm-show changes commands continuation)
                'pending)))))))))

;;; Comments & Activity sync

(defun orgist-insert-comment-as-note (posted-at content)
  "Insert a Todoist comment as a logbook Note entry.
POSTED-AT is an ISO 8601 timestamp string.  CONTENT is the
comment text (may be multi-line, markdown).  Point must be on
the heading."
  (let* ((timestamp (orgist-parse-todoist-timestamp posted-at))
         (level (org-current-level))
         (converted (if (and content (not (string-empty-p content)))
                        (orgist-convert-description content (or level 1))
                      ""))
         (log-beginning (orgist--log-insertion-point)))
    (save-excursion
      (goto-char log-beginning)
      (let ((itemp (org-in-item-p)))
        (if itemp
            (indent-line-to
             (let ((struct (save-excursion
                             (goto-char itemp) (org-list-struct))))
               (org-list-get-ind (org-list-get-top-point struct) struct)))
          (org-indent-line)))
      (let* ((lines (split-string converted "\n"))
             (first-line (or (car lines) ""))
             (rest-lines (cdr lines)))
        (insert-and-inherit
         (org-list-bullet-string "-")
         (format "%s Note \\\\\n" (or timestamp "[unknown]")))
        ;; Insert content lines with proper indentation
        (let ((indent (make-string 2 ?\s)))
          (insert indent first-line "\n")
          (dolist (line rest-lines)
            (insert indent line "\n")))))))

(defun orgist-insert-activity-as-log (event)
  "Insert a Todoist activity EVENT as a logbook state-change entry.
Only handles `completed' and `uncompleted' events — these mirror
task state transitions in the org logbook.  Comments are pulled
separately via the comments API; `updated' events are noise.
EVENT is an alist with keys from the Todoist API (v1 camelCase
or legacy snake_case).  Point must be on the heading."
  (let* ((event-type (or (alist-get 'event_type event)
                         (alist-get 'eventType event)))
         (event-date (or (alist-get 'event_date event)
                         (alist-get 'eventDate event)))
         (timestamp (orgist-parse-todoist-timestamp event-date)))
    (when timestamp
      (cond
       ((string= event-type "completed")
        (orgist-insert-log-entry "DONE" "TODO" timestamp))
       ((string= event-type "uncompleted")
        (orgist-insert-log-entry "TODO" "DONE" timestamp))))))

(defun orgist--http-error-code (error-thrown)
  "Extract HTTP status code from ERROR-THROWN.
ERROR-THROWN may be (error . \"http NNN\") or (error http NNN).
Returns the integer status code, or nil if not an HTTP error."
  (when (consp error-thrown)
    (let ((rest (cdr error-thrown)))
      (cond
       ;; (error . "http 410") — string cdr
       ((and (stringp rest)
             (string-match "\\`http \\([0-9]+\\)" rest))
        (string-to-number (match-string 1 rest)))
       ;; (error "http 410") — one-element list with string
       ((and (consp rest) (stringp (car rest))
             (string-match "\\`http \\([0-9]+\\)" (car rest)))
        (string-to-number (match-string 1 (car rest))))
       ;; (error http 410) — symbol + number
       ((and (consp rest)
             (eq (car rest) 'http)
             (numberp (cadr rest)))
        (cadr rest))))))

(defvar orgist-request-retry-max 3
  "Maximum number of retries for transient HTTP and transport errors.")

(defun orgist--transport-error-p (error-thrown)
  "Return non-nil if ERROR-THROWN is a retryable transport error.
Detects curl exit codes (SSL failures, timeouts, connection refused)
and other non-HTTP errors from `request.el'."
  (when (consp error-thrown)
    (let ((msg (cond
                ((stringp (cdr error-thrown)) (cdr error-thrown))
                ((and (consp (cdr error-thrown))
                      (stringp (cadr error-thrown)))
                 (cadr error-thrown)))))
      (and msg (string-match-p "\\(?:exited abnormally\\|peculiar error\\)" msg)))))

(defun orgist--retryable-http-p (code)
  "Return non-nil if HTTP status CODE is retryable."
  (and code (memq code '(429 500 502 503 504))))

(defun orgist--request-with-retry (url &rest args)
  "Make a request to URL with ARGS, retrying on transient errors.
Retries on HTTP 429/5xx (server errors) and transport errors
\(curl SSL, timeout, connection failures).  Uses the `retry_after'
header for 429, exponential backoff for other errors, up to
`orgist-request-retry-max' attempts.  The caller's :success and
:error callbacks work as usual."
  (let ((retries 0)
        (done nil))
    (while (not done)
      (let* ((should-retry nil)
             (retry-after 5)
             (orig-error (plist-get args :error))
             (wrapped-error
              (cl-function
               (lambda (&key data error-thrown &allow-other-keys)
                 (let ((code (orgist--http-error-code error-thrown))
                       (transport (orgist--transport-error-p error-thrown)))
                   (cond
                    ;; HTTP 429 — use server's retry_after
                    ((and (eql code 429) (< retries orgist-request-retry-max))
                     (setq should-retry t)
                     (when-let* ((ra (alist-get 'retry_after data)))
                       (setq retry-after ra))
                     (orgist-log 'warn "Rate limited, retrying in %ds (%d/%d)"
                                 retry-after (1+ retries)
                                 orgist-request-retry-max))
                    ;; HTTP 5xx — exponential backoff
                    ((and (orgist--retryable-http-p code)
                          (< retries orgist-request-retry-max))
                     (setq should-retry t
                           retry-after (* 2 (1+ retries)))
                     (orgist-log 'warn "HTTP %d, retrying in %ds (%d/%d)"
                                 code retry-after (1+ retries)
                                 orgist-request-retry-max))
                    ;; Transport error (curl SSL, timeout, etc.)
                    ((and transport (< retries orgist-request-retry-max))
                     (setq should-retry t
                           retry-after (* 2 (1+ retries)))
                     (orgist-log 'warn "Transport error, retrying in %ds (%d/%d): %s"
                                 retry-after (1+ retries)
                                 orgist-request-retry-max
                                 (if (consp error-thrown)
                                     (or (cdr error-thrown) (cadr error-thrown))
                                   error-thrown)))
                    ;; Not retryable or retries exhausted
                    (t
                     (when (and (> retries 0)
                                (or code transport))
                       (orgist-log 'warn "Request failed after %d retries: %s"
                                   retries
                                   (if code (format "HTTP %d" code)
                                     (format "%S" error-thrown))))
                     (when orig-error
                       (funcall orig-error
                                :data data
                                :error-thrown error-thrown)))))))))
        (setq args (plist-put args :error wrapped-error))
        (apply #'request url args)
        (if should-retry
            (progn
              (setq retries (1+ retries))
              (sleep-for retry-after))
          (setq done t))))))

(defun orgist-fetch-plan-limits ()
  "Fetch user plan limits from the Todoist Sync API.
Caches the result in `orgist-plan-limits'.  Returns the alist.
Key fields: `activity_log' (boolean), `activity_log_limit' (integer)."
  (or orgist-plan-limits
      (progn
        (orgist--request-with-retry
          "https://api.todoist.com/api/v1/sync"
          :headers `(("Authorization" . ,(format "Bearer %s" orgist-bearer-token)))
          :data `(("sync_token" . "*")
                  ("resource_types" . "[\"user_plan_limits\"]"))
          :parser 'json-read
          :sync t
          :error (cl-function
                  (lambda (&key data error-thrown &allow-other-keys)
                    (orgist-log 'warn "Plan limits API error: %S (err=%S)"
                                data error-thrown)))
          :success (cl-function
                    (lambda (&key data &allow-other-keys)
                      (setq orgist-plan-limits
                            (alist-get 'user_plan_limits data)))))
        orgist-plan-limits)))

(defun orgist-activity-log-available-p ()
  "Return non-nil if the user's plan supports the activity log."
  (let ((limits (orgist-fetch-plan-limits)))
    (and limits
         (not (eq (alist-get 'activity_log limits) :json-false)))))

(defun orgist-completed-tasks-available-p ()
  "Return non-nil if the user's plan supports completed tasks."
  (let ((limits (orgist-fetch-plan-limits)))
    (and limits
         (not (eq (alist-get 'completed_tasks limits) :json-false)))))

;;; Archived sections

(defun orgist--fetch-archived-sections-for-project (project-id)
  "Fetch all archived sections for PROJECT-ID.
Paginates via cursor.  Returns a flat list of section alists."
  (let ((all-sections '())
        (cursor nil)
        (done nil))
    (while (not done)
      (let ((page-sections nil)
            (next-cursor nil)
            (params `(("project_id" . ,project-id))))
        (when cursor
          (push (cons "cursor" cursor) params))
        (orgist--request-with-retry
          "https://api.todoist.com/api/v1/sections/archived"
          :type "GET"
          :headers `(("Authorization"
                      . ,(format "Bearer %s" orgist-bearer-token)))
          :params params
          :parser 'json-read
          :sync t
          :error (cl-function
                  (lambda (&key error-thrown &allow-other-keys)
                    (let ((code (orgist--http-error-code error-thrown)))
                      (unless (memq code '(403 404))
                        (orgist-log 'warn "Archived sections error for %s: %S"
                                    project-id error-thrown)))
                    (setq done t)))
          :success (cl-function
                    (lambda (&key data &allow-other-keys)
                      (setq page-sections
                            (cond
                             ((and (listp data) (assq 'sections data))
                              (append (alist-get 'sections data) nil))
                             ((and (listp data) (assq 'results data))
                              (append (alist-get 'results data) nil))
                             ((vectorp data) (append data nil))
                             (t nil)))
                      (setq next-cursor
                            (and (listp data) (alist-get 'next_cursor data))))))
        (setq all-sections (nconc all-sections page-sections))
        (if (or (null page-sections) (null next-cursor))
            (setq done t)
          (setq cursor next-cursor))))
    all-sections))

(defun orgist-fetch-archived-sections ()
  "Fetch archived sections for all synced projects.
Calls GET /sections/archived?project_id=X for each unique project
that has a buffer in `orgist-base-dir'.  Returns a flat list of
section alists."
  (let ((all-sections '())
        (seen-projects (make-hash-table :test 'equal)))
    (dolist (file (directory-files orgist-base-dir t "\\`[^.].*\\.org\\'"))
      (when-let* ((buf (find-buffer-visiting file)))
        (with-current-buffer buf
          (when-let* ((project-id (org-entry-get (point-min) "ID" t)))
            (unless (gethash project-id seen-projects)
              (puthash project-id t seen-projects)
              (let ((sections (orgist--fetch-archived-sections-for-project
                               project-id)))
                (when sections
                  (orgist-log 'debug "Archived sections: %d in project %s"
                              (length sections) project-id)
                  (setq all-sections (nconc all-sections sections)))))))))
    all-sections))

;;; Completed tasks

(defun orgist--fetch-completed-tasks-chunk (since until &optional project-id)
  "Fetch completed tasks for a single date range (max 3 months).
SINCE and UNTIL are ISO 8601 strings.  Returns a flat list of task alists.
Paginates via cursor."
  (let ((all-tasks nil)
        (cursor nil)
        (done nil))
    (while (not done)
      (let ((page-tasks nil)
            (next-cursor nil)
            (params `(("since" . ,since)
                      ("until" . ,until)
                      ("limit" . "200"))))
        (when project-id
          (push (cons "project_id" project-id) params))
        (when cursor
          (push (cons "cursor" cursor) params))
        (orgist--request-with-retry
          "https://api.todoist.com/api/v1/tasks/completed/by_completion_date"
          :type "GET"
          :headers `(("Authorization" . ,(format "Bearer %s" orgist-bearer-token)))
          :params params
          :parser 'json-read
          :sync t
          :error (cl-function
                  (lambda (&key data error-thrown symbol-status &allow-other-keys)
                    (let ((code (orgist--http-error-code error-thrown)))
                      (if (memq code '(403 404 410))
                          (orgist-log 'debug "Completed tasks: HTTP %s, skipping" code)
                        (orgist-log 'warn "Completed tasks API error: %S (status=%S err=%S)"
                                    data symbol-status error-thrown)))
                    (setq done t)))
          :success (cl-function
                    (lambda (&key data &allow-other-keys)
                      (let ((tasks (cond
                                    ((and (listp data) (assq 'results data))
                                     (alist-get 'results data))
                                    ((and (listp data) (assq 'items data))
                                     (alist-get 'items data))
                                    (t data))))
                        (setq page-tasks (append tasks nil))
                        (setq next-cursor (alist-get 'next_cursor data))))))
        (setq all-tasks (nconc all-tasks page-tasks))
        (if (or (null page-tasks) (null next-cursor))
            (setq done t)
          (setq cursor next-cursor))))
    all-tasks))

(defun orgist-fetch-completed-tasks (since until &optional project-id)
  "Fetch completed tasks from Todoist between SINCE and UNTIL.
SINCE and UNTIL are ISO 8601 strings.  Optional PROJECT-ID limits
to a single project.  Returns a flat list of task alists.
Automatically chunks into 90-day windows to stay within the API's
3-month date range limit."
  (let* ((since-time (date-to-time since))
         (until-time (date-to-time until))
         (chunk-seconds (* 90 86400))
         (all-tasks nil)
         (chunk-start since-time)
         (chunk-num 0))
    (while (time-less-p chunk-start until-time)
      (let* ((chunk-end (time-add chunk-start chunk-seconds))
             (chunk-end (if (time-less-p until-time chunk-end) until-time chunk-end))
             (since-str (format-time-string "%Y-%m-%dT%H:%M:%SZ" chunk-start t))
             (until-str (format-time-string "%Y-%m-%dT%H:%M:%SZ" chunk-end t)))
        (setq chunk-num (1+ chunk-num))
        (orgist-log 'debug "Completed tasks: chunk %d (%s to %s)"
                    chunk-num since-str until-str)
        (let ((tasks (orgist--fetch-completed-tasks-chunk
                      since-str until-str project-id)))
          (setq all-tasks (nconc all-tasks tasks)))
        (setq chunk-start chunk-end)))
    all-tasks))

(defun orgist--normalize-completed-task (task)
  "Normalize a completed task alist for `orgist-update-elements'.
Maps REST API field names to Sync API names and ensures checked=t."
  (let ((normalized (copy-alist task)))
    (unless (assq 'child_order normalized)
      (push (cons 'child_order (or (alist-get 'order normalized) 0)) normalized))
    (unless (assq 'added_at normalized)
      (push (cons 'added_at (alist-get 'created_at normalized)) normalized))
    (setf (alist-get 'checked normalized) t)
    normalized))

(defun orgist--sort-completed-tasks-parents-first (tasks)
  "Sort TASKS so that parent tasks come before their children.
This ensures that when inserting completed subtasks, their parent
task has already been inserted into the org buffer."
  (let* ((id-set (make-hash-table :test 'equal))
         (result '())
         (remaining (copy-sequence tasks))
         (max-passes (1+ (length tasks)))
         (pass 0))
    ;; Build set of task IDs in this batch
    (dolist (task tasks)
      (puthash (alist-get 'id task) t id-set))
    ;; Iteratively emit tasks whose parent is not in the remaining set
    ;; (or whose parent has already been emitted).
    (while (and remaining (< pass max-passes))
      (setq pass (1+ pass))
      (let ((next-remaining '()))
        (dolist (task remaining)
          (let ((pid (alist-get 'parent_id task)))
            (if (or (null pid)
                    (not (gethash pid id-set)))
                ;; Parent already emitted or not in batch — emit now
                (progn
                  (push task result)
                  (remhash (alist-get 'id task) id-set))
              ;; Parent still pending — defer
              (push task next-remaining))))
        (if (= (length next-remaining) (length remaining))
            ;; No progress — break cycle by emitting all remaining
            (progn
              (setq result (nconc (nreverse next-remaining) result))
              (setq remaining nil))
          (setq remaining (nreverse next-remaining)))))
    (nreverse result)))

(defun orgist--process-completed-task (normalized)
  "Try to insert or update a single completed task NORMALIZED.
Returns `inserted', `updated', or nil if the parent was not found."
  (let* ((task-id (alist-get 'id normalized))
         (project-id (alist-get 'project_id normalized))
         (section-id (alist-get 'section_id normalized))
         (parent-id (alist-get 'parent_id normalized))
         (order (or (alist-get 'child_order normalized) 0))
         (project-buffer (orgist-get-project-buffer project-id)))
    (if (not project-buffer)
        (progn
          (orgist-log 'warn "Completed: no buffer for project %s, skipping %s"
                      project-id (alist-get 'content normalized))
          'skipped)
      (with-current-buffer project-buffer
        (let ((result
               (save-excursion
                 (let ((existing (orgist-find-element-by-id task-id)))
                   (if existing
                       (progn
                         (goto-char existing)
                         (orgist-update-element normalized)
                         (org-set-property "TODOIST-ORDER"
                                           (number-to-string order))
                         'updated)
                     ;; New task — find parent, falling back through
                     ;; parent → section → project
                     (let* ((candidates (delq nil (list parent-id section-id project-id)))
                            (parent-point
                             (cl-some #'orgist-find-element-by-id candidates)))
                       (if parent-point
                           (progn
                             (goto-char parent-point)
                             (orgist-insert-element normalized)
                             (orgist-update-element normalized t)
                             (org-set-property "TODOIST-ORDER"
                                               (number-to-string order))
                             'inserted)
                         nil)))))))
          (orgist--save-buffer)
          result)))))

(defun orgist--completed-retry-file ()
  "Return path to the file storing completed tasks awaiting retry."
  (expand-file-name "completed-retry.json" orgist-base-dir))

(defun orgist--load-completed-retries ()
  "Load completed tasks that could not be placed by earlier pulls."
  (let ((file (orgist--completed-retry-file)))
    (when (file-exists-p file)
      (condition-case err
          (let ((json-object-type 'alist)
                (json-key-type 'symbol)
                (json-array-type 'list))
            (json-read-file file))
        (error
         (orgist-log 'warn "Completed: dropping unreadable retry file: %S" err)
         nil)))))

(defun orgist--save-completed-retries (tasks)
  "Persist unplaced completed TASKS for the next pull; delete file when none."
  (let ((file (orgist--completed-retry-file)))
    (if tasks
        (with-temp-file file
          ;; vconcat forces a JSON array — a bare list of alists is
          ;; ambiguous to `json-encode'.
          (insert (json-encode (vconcat tasks))))
      (when (file-exists-p file)
        (delete-file file)))))

(defun orgist-process-completed-tasks (tasks)
  "Insert or update completed TASKS into their project org buffers.
Each task is normalized and then inserted/updated using the same
machinery as `orgist-update-elements'.  Tasks are sorted so parents
are processed before children.  Tasks whose parent is not yet
present are deferred and retried in subsequent passes, since
inserting a parent in pass N makes its children resolvable in
pass N+1.  Tasks that still cannot be placed (their project buffer
or parent is unresolvable) are persisted and retried on later
pulls, up to `orgist-completed-retry-attempts' times, so a
transient resolution failure does not silently lose the completion."
  (let* ((carried (seq-remove
                   (lambda (retry)
                     (let ((rid (alist-get 'id retry)))
                       (seq-some (lambda (tk) (equal (alist-get 'id tk) rid))
                                 tasks)))
                   (orgist--load-completed-retries)))
         (sorted (orgist--sort-completed-tasks-parents-first
                  (append tasks carried)))
         (total (length sorted))
         (inserted 0)
         (updated 0)
         (skipped 0)
         (unplaced '())
         (remaining sorted)
         (pass 0))
    (when carried
      (orgist-log 'debug "Completed tasks: retrying %d previously unplaced task(s)"
                  (length carried)))
    (while remaining
      (setq pass (1+ pass))
      (let ((deferred '())
            (count 0)
            (progress-made nil))
        (dolist (task remaining)
          (setq count (1+ count))
          (when (and (= pass 1) (= (% count 50) 0))
            (orgist-log 'debug "Completed tasks: [%d/%d]" count total))
          (let* ((normalized (orgist--normalize-completed-task task))
                 (result (orgist--process-completed-task normalized)))
            (pcase result
              ('inserted (setq inserted (1+ inserted))
                         (setq progress-made t))
              ('updated  (setq updated (1+ updated))
                         (setq progress-made t))
              ('skipped  (setq skipped (1+ skipped))
                         (push task unplaced))
              ('nil      (push task deferred)))))
        (if (and deferred (not progress-made))
            ;; No progress — give up on remaining tasks
            (progn
              (dolist (task deferred)
                (let ((name (alist-get 'content task))
                      (pid (alist-get 'parent_id task)))
                  (orgist-log 'warn "Completed: parent %s not found for %s, skipping"
                              pid name))
                (setq skipped (1+ skipped))
                (push task unplaced))
              (setq remaining nil))
          (when deferred
            (orgist-log 'debug "Completed tasks: pass %d deferred %d tasks, retrying"
                        pass (length deferred)))
          (setq remaining (nreverse deferred)))))
    ;; Persist unplaced tasks for the next pull; drop after too many
    ;; attempts (e.g. tasks of archived projects are never placeable).
    (orgist--save-completed-retries
     (let (keep)
       (dolist (task unplaced (nreverse keep))
         (let ((attempts (1+ (or (alist-get 'orgist_retry task) 0))))
           (if (> attempts orgist-completed-retry-attempts)
               (orgist-log 'warn "Completed: giving up on %s after %d attempts"
                           (alist-get 'content task) (1- attempts))
             (setf (alist-get 'orgist_retry task) attempts)
             (push task keep))))))
    (orgist-log (if (or (> inserted 0) (> updated 0)) 'info 'debug)
                "Completed tasks: %d processed (%d inserted, %d updated, %d skipped)"
                total inserted updated skipped)))

;;; Completed tasks timestamp persistence

(defun orgist--completed-pull-timestamp-file ()
  "Return path to the file storing last completed-tasks pull timestamp."
  (expand-file-name "completed_pull_timestamp" orgist-base-dir))

(defun orgist-save-completed-pull-timestamp ()
  "Save current time as the last completed-tasks pull timestamp."
  (with-temp-file (orgist--completed-pull-timestamp-file)
    (insert (format-time-string "%Y-%m-%dT%H:%M:%SZ" nil t))))

(defun orgist-load-completed-pull-timestamp ()
  "Load the last completed-tasks pull timestamp, or nil if none."
  (let ((file (orgist--completed-pull-timestamp-file)))
    (when (file-exists-p file)
      (string-trim (with-temp-buffer
                     (insert-file-contents file) (buffer-string))))))

(defun orgist-fetch-task-comments (task-id)
  "Fetch comments for TASK-ID from the Todoist API.
Returns a list of comment alists, or nil on error.
HTTP 403/404/410 are treated as empty (task gone or inaccessible).
Retries automatically on HTTP 429 rate limiting."
  (let ((result nil))
    (orgist--request-with-retry
      (format "https://api.todoist.com/api/v1/comments?task_id=%s" task-id)
      :headers `(("Authorization" . ,(format "Bearer %s" orgist-bearer-token)))
      :parser 'json-read
      :sync t
      :error (cl-function
              (lambda (&key data error-thrown symbol-status &allow-other-keys)
                (let ((code (orgist--http-error-code error-thrown)))
                  (if (memq code '(403 404 410))
                      (orgist-log 'debug "Comments: %s returned HTTP %s, skipping"
                                  task-id code)
                    (orgist-log 'warn "Comments API error for %s: %S (status=%S err=%S)"
                                task-id data symbol-status error-thrown)))))
      :success (cl-function
                (lambda (&key data &allow-other-keys)
                  ;; API v1 wraps comments in {"results": [...]}
                  (let ((comments (if (and (listp data) (assq 'results data))
                                      (alist-get 'results data)
                                    data)))
                    (setq result (append comments nil))))))
    result))

(defun orgist-fetch-task-activity (task-id)
  "Fetch activity events for TASK-ID from the Todoist API v1.
Returns a list of activity event alists.  Paginates via cursor.
Returns nil gracefully when the plan does not support activity
or the endpoint returns HTTP 403/404/410.
Only returns completed/uncompleted events (state changes)."
  (let ((all-events nil)
        (cursor nil)
        (limit 100)
        (done nil))
    (while (not done)
      (let ((page-events nil)
            (next-cursor nil)
            ;; API expects snake_case query parameters (the TypeScript
            ;; SDK auto-converts via axios-case-converter).
            (params `(("object_type" . "item")
                      ("object_id" . ,task-id)
                      ("limit" . ,(number-to-string limit)))))
        (when cursor
          (push (cons "cursor" cursor) params))
        (orgist--request-with-retry
          "https://api.todoist.com/api/v1/activities"
          :headers `(("Authorization" . ,(format "Bearer %s" orgist-bearer-token)))
          :params params
          :parser 'json-read
          :sync t
          :error (cl-function
                  (lambda (&key data error-thrown symbol-status &allow-other-keys)
                    (let ((code (orgist--http-error-code error-thrown)))
                      (if (memq code '(403 404 410))
                          (orgist-log 'debug "Activity: %s returned HTTP %s, skipping"
                                      task-id code)
                        (orgist-log 'warn "Activity API error for %s: %S (status=%S err=%S)"
                                    task-id data symbol-status error-thrown)))
                    (setq done t)))
          :success (cl-function
                    (lambda (&key data &allow-other-keys)
                      ;; v1 wraps in {"results": [...], "nextCursor": ...}
                      (let ((events (if (and (listp data) (assq 'results data))
                                        (alist-get 'results data)
                                      ;; Fallback for old format in test fixtures
                                      (alist-get 'events data))))
                        (setq page-events (append events nil))
                        (setq next-cursor (alist-get 'nextCursor data))))))
        (setq all-events (nconc all-events page-events))
        (if (or (null page-events) (null next-cursor))
            (setq done t)
          (setq cursor next-cursor))))
    ;; Client-side safety filter: only state-change events for this task.
    ;; Guards against API returning unfiltered results (e.g. wrong param names).
    (seq-filter
     (lambda (ev)
       (let ((oid (or (alist-get 'objectId ev) (alist-get 'object_id ev)))
             (etype (or (alist-get 'eventType ev) (alist-get 'event_type ev))))
         (and (equal oid task-id)
              (member etype '("completed" "uncompleted")))))
     all-events)))

(defun orgist-sync-task-comments-and-activity (task-id)
  "Sync comments and activity for a single TASK-ID.
Fetches from the API, filters out already-known entries, inserts
new ones into the logbook, and updates the snapshot.
Searches all open org buffers for the task heading."
  (let ((pos nil)
        (target-buf nil))
    ;; Search all org buffers for the task
    (catch 'found
      (dolist (buf (buffer-list))
        (when (and (buffer-file-name buf)
                   (string-match-p "\\.org\\'" (buffer-file-name buf)))
          (with-current-buffer buf
            (when-let* ((p (orgist-find-element-by-id task-id)))
              ;; Use a marker so pos tracks buffer modifications
              ;; from comment/activity inserts.
              (setq pos (copy-marker p t) target-buf buf)
              (throw 'found nil))))))
    (if (not pos)
        (orgist-log 'debug "Comments: task %s not found in any buffer, skipping" task-id)
    (unwind-protect
    (with-current-buffer target-buf
      (save-excursion
        (save-restriction
          (widen)
          (goto-char pos)
          (org-back-to-heading t)
          (let* ((snapshot (gethash task-id orgist-snapshots))
                 (known-comment-ids (when snapshot
                                      (plist-get snapshot :comment-ids)))
                 (known-activity-ids (when snapshot
                                       (plist-get snapshot :activity-ids)))
                 (new-comment-ids (copy-sequence (or known-comment-ids '())))
                 (new-activity-ids (copy-sequence (or known-activity-ids '())))
                 ;; Fetch comments (skip if note_count is 0).
                 ;; Sort chronologically (oldest first) so entries are
                 ;; inserted in the correct order regardless of
                 ;; `org-log-states-order-reversed'.
                 (note-count (when snapshot (plist-get snapshot :note-count)))
                 (comments (if (and note-count (= note-count 0))
                               (progn
                                 (orgist-log 'debug "Comments: skipping %s (note_count=0)" task-id)
                                 nil)
                             (orgist-fetch-task-comments task-id)))
                 (comments (sort (copy-sequence comments)
                                 (lambda (a b)
                                   (string< (or (alist-get 'posted_at a) "")
                                            (or (alist-get 'posted_at b) ""))))))
            (dolist (comment comments)
              (let ((comment-id (alist-get 'id comment)))
                (cond
                 ;; Metadata comment — restore properties, don't insert as Note
                 ((orgist--metadata-comment-p comment)
                  (when orgist-sync-metadata
                    (orgist-log 'debug "Restoring metadata from comment %s" comment-id)
                    (condition-case err
                        (save-excursion
                          (goto-char pos)
                          (org-back-to-heading t)
                          (orgist-restore-metadata (alist-get 'content comment)))
                      (error
                       (orgist-log 'warn "Failed to restore metadata %s: %s"
                                   comment-id (error-message-string err)))))
                  ;; Track it so we don't re-process, and store the ID
                  (push comment-id new-comment-ids)
                  (when snapshot
                    (plist-put snapshot :metadata-comment-id comment-id)))
                 ;; Already known comment — skip
                 ((member comment-id known-comment-ids) nil)
                 ;; New regular comment — insert as logbook Note
                 (t
                  (orgist-log 'debug "Comments: inserting comment %s for task %s"
                              comment-id task-id)
                  (condition-case err
                      (save-excursion
                        (goto-char pos)
                        (org-back-to-heading t)
                        (orgist-insert-comment-as-note
                         (alist-get 'posted_at comment)
                         (alist-get 'content comment)))
                    (error
                     (orgist-log 'warn "Failed to insert comment %s: %s"
                                 comment-id (error-message-string err))))
                  (push comment-id new-comment-ids)))))
            ;; Fetch and insert activity (completed/uncompleted only).
            ;; Sort chronologically so insertion order is correct.
            (let ((events (when (orgist-activity-log-available-p)
                            (sort (copy-sequence
                                   (orgist-fetch-task-activity task-id))
                                  (lambda (a b)
                                    (string< (or (alist-get 'event_date a)
                                                 (alist-get 'eventDate a) "")
                                             (or (alist-get 'event_date b)
                                                 (alist-get 'eventDate b) "")))))))
              (dolist (event events)
                (let ((event-id (alist-get 'id event)))
                  (unless (member event-id known-activity-ids)
                    (orgist-log 'debug "Activity: inserting %s for task %s"
                                event-id task-id)
                    (condition-case err
                        (save-excursion
                          (goto-char pos)
                          (org-back-to-heading t)
                          (orgist-insert-activity-as-log event))
                      (error
                       (orgist-log 'warn "Failed to insert activity %s: %s"
                                   event-id (error-message-string err))))
                    (push event-id new-activity-ids))))
              ;; Deduplicate and sort logbook.  Each duplicate Note
              ;; removed here was a known note re-inserted because its
              ;; snapshot id was a "rebuilt" placeholder — its real id
              ;; was just recorded above, so retire one placeholder per
              ;; removal to keep the known-comment count aligned with
              ;; the actual logbook notes.
              (condition-case nil
                  (save-excursion
                    (goto-char pos)
                    (org-back-to-heading t)
                    (let* ((removed-notes (or (orgist-deduplicate-logbook) 0))
                           (placeholders (seq-count
                                          (lambda (i) (equal i "rebuilt"))
                                          new-comment-ids))
                           (keep (- placeholders
                                    (min removed-notes placeholders))))
                      (when (< keep placeholders)
                        (setq new-comment-ids
                              (append (seq-remove
                                       (lambda (i) (equal i "rebuilt"))
                                       new-comment-ids)
                                      (make-list keep "rebuilt")))))
                    (orgist-sort-logbook))
                (error nil))
              ;; Sync attachments from comments (download new, remove stale).
              (let ((new-attachment-files
                     (when orgist-sync-attachments
                       (condition-case err
                           (save-excursion
                             (goto-char pos)
                             (org-back-to-heading t)
                             (orgist-sync-task-attachments
                              task-id comments
                              (when snapshot
                                (plist-get snapshot :attachment-files))))
                         (error
                          (orgist-log 'warn "Failed to sync attachments for %s: %s"
                                      task-id (error-message-string err))
                          (when snapshot
                            (plist-get snapshot :attachment-files)))))))
                ;; Update snapshot — always update even if insertion partially
                ;; failed, to avoid retrying the same entries indefinitely.
                (when snapshot
                  (let ((updated (copy-sequence snapshot)))
                    (plist-put updated :comment-ids new-comment-ids)
                    (plist-put updated :activity-ids new-activity-ids)
                    (plist-put updated :comments-pulled t)
                    (when orgist-sync-attachments
                      (plist-put updated :attachment-files new-attachment-files))
                    (puthash task-id updated orgist-snapshots)))))))))
      ;; Clean up marker
      (when (markerp pos) (set-marker pos nil))))))

(defun orgist-download-file (url dest-path)
  "Download file from URL to DEST-PATH.
Uses `url-copy-file' for simplicity.
Returns DEST-PATH on success, nil on error."
  (condition-case err
      (progn
        (url-copy-file url dest-path t)
        dest-path)
    (error
     (orgist-log 'warn "Failed to download %s: %s" url (error-message-string err))
     nil)))

(defun orgist-upload-file (file-path)
  "Upload FILE-PATH to Todoist via the uploads API.
Returns the file attachment alist (file_url, file_name, file_type, file_size)
on success, or nil on error."
  (let ((result nil)
        (file-name (file-name-nondirectory file-path)))
    (orgist--request-with-retry
      "https://api.todoist.com/api/v1/uploads"
      :type "POST"
      :headers `(("Authorization" . ,(format "Bearer %s" orgist-bearer-token)))
      :files `(("file" . ,file-path))
      :parser 'json-read
      :sync t
      :error (cl-function
              (lambda (&key data error-thrown &allow-other-keys)
                (orgist-log 'warn "Upload failed for %s: %S (err=%S)"
                            file-name data error-thrown)))
      :success (cl-function
                (lambda (&key data &allow-other-keys)
                  (setq result data))))
    result))

(defun orgist-create-comment-with-attachment (task-id attachment-meta)
  "Create a Todoist comment on TASK-ID with file ATTACHMENT-META.
ATTACHMENT-META is the alist returned by `orgist-upload-file'.
Returns the created comment alist, or nil on error."
  (let ((result nil))
    (orgist--request-with-retry
      "https://api.todoist.com/api/v1/comments"
      :type "POST"
      :headers `(("Authorization" . ,(format "Bearer %s" orgist-bearer-token))
                 ("Content-Type" . "application/json"))
      :data (json-encode
             `((task_id . ,task-id)
               (content . ,(or (alist-get 'file_name attachment-meta) ""))
               (attachment
                . ((resource_type . "file")
                   (file_url . ,(alist-get 'file_url attachment-meta))
                   (file_type . ,(alist-get 'file_type attachment-meta))
                   (file_name . ,(alist-get 'file_name attachment-meta))))))
      :parser 'json-read
      :sync t
      :error (cl-function
              (lambda (&key data error-thrown &allow-other-keys)
                (orgist-log 'warn "Comment create failed for task %s: %S (err=%S)"
                            task-id data error-thrown)))
      :success (cl-function
                (lambda (&key data &allow-other-keys)
                  (setq result data))))
    result))

(defun orgist-delete-comment (comment-id)
  "Delete a Todoist comment by COMMENT-ID.
Returns non-nil on success."
  (let ((ok nil))
    (orgist--request-with-retry
      (format "https://api.todoist.com/api/v1/comments/%s" comment-id)
      :type "DELETE"
      :headers `(("Authorization" . ,(format "Bearer %s" orgist-bearer-token)))
      :sync t
      :error (cl-function
              (lambda (&key data error-thrown &allow-other-keys)
                (let ((code (orgist--http-error-code error-thrown)))
                  (if (memq code '(404 410))
                      (progn (orgist-log 'debug "Comment %s already gone" comment-id)
                             (setq ok t))
                    (orgist-log 'warn "Comment delete failed for %s: %S (err=%S)"
                                comment-id data error-thrown)))))
      :success (cl-function
                (lambda (&rest _) (setq ok t))))
    ok))

;;; Metadata comments (non-Todoist property preservation)

(defconst orgist-metadata-marker "[orgist-metadata]"
  "Marker string identifying a metadata preservation comment.")

(defun orgist--metadata-comment-p (comment)
  "Return non-nil if COMMENT is an orgist metadata comment."
  (let ((content (alist-get 'content comment)))
    (and content (string-prefix-p orgist-metadata-marker content))))

(defun orgist-extract-metadata ()
  "Extract non-Todoist metadata from the heading at point.
Returns a string to store as a metadata comment, or nil if
no non-Todoist metadata exists.  Point must be on the heading."
  (org-back-to-heading-or-point-min t)
  (let ((parts '()))
    ;; 1. Non-Todoist properties
    (let* ((all-props (org-entry-properties nil 'standard))
           (extra-props
            (seq-remove
             (lambda (pair)
               (member-ignore-case (car pair) orgist--managed-properties))
             all-props)))
      (when extra-props
        (push ":PROPERTIES:" parts)
        (dolist (prop extra-props)
          (push (format ":%s: %s" (car prop) (cdr prop)) parts))
        (push ":END:" parts)))
    ;; 2. Custom drawers (not PROPERTIES or LOGBOOK)
    (save-excursion
      (org-back-to-heading t)
      (let* ((subtree-end (orgist--subtree-end))
             (search-start (progn (forward-line 1) (point)))
             (search-end (save-excursion
                           (goto-char search-start)
                           (if (re-search-forward
                                org-outline-regexp-bol subtree-end t)
                               (line-beginning-position)
                             subtree-end))))
        (goto-char search-start)
        (while (re-search-forward
                "^[ \t]*:\\([A-Za-z][A-Za-z0-9_-]*\\):[ \t]*$"
                search-end t)
          (let ((drawer-name (match-string 1)))
            (unless (member-ignore-case drawer-name
                                        '("PROPERTIES" "LOGBOOK" "END"))
              (let ((drawer-start (line-beginning-position)))
                (when (re-search-forward "^[ \t]*:END:" search-end t)
                  (let ((drawer-text
                         (buffer-substring-no-properties
                          drawer-start (line-end-position))))
                    (push drawer-text parts)))))))))
    (when parts
      (concat orgist-metadata-marker "\n"
              (string-join (nreverse parts) "\n")))))

(defun orgist-restore-metadata (metadata-text)
  "Restore non-Todoist metadata to the heading at point.
METADATA-TEXT is the content of an [orgist-metadata] comment.
Only restores properties that don't already exist locally.
Point must be on the heading."
  (when (and metadata-text (string-prefix-p orgist-metadata-marker metadata-text))
    (let ((text (substring metadata-text (1+ (length orgist-metadata-marker)))))
      (org-back-to-heading-or-point-min t)
      ;; 1. Restore properties
      (when (string-match ":PROPERTIES:\n\\(\\(?::.+\n\\)*\\):END:" text)
        (let ((prop-block (match-string 1 text)))
          (dolist (line (split-string prop-block "\n" t))
            (when (string-match "^:\\([^:]+\\):\\s-*\\(.*\\)" line)
              (let ((key (match-string 1 line))
                    (val (match-string 2 line)))
                ;; Only restore if not managed and not already set
                (unless (or (member-ignore-case key orgist--managed-properties)
                            (org-entry-get (point) key))
                  (org-entry-put (point) key val)))))))
      ;; 2. Restore custom drawers
      (let ((pos 0))
        (while (string-match
                "^:\\([A-Za-z][A-Za-z0-9_-]*\\):[ \t]*\n\\(\\(?:.*\n\\)*?\\):END:"
                text pos)
          (let ((drawer-name (match-string 1 text))
                (drawer-text (match-string 0 text)))
            (setq pos (match-end 0))
            (unless (member-ignore-case drawer-name
                                        '("PROPERTIES" "LOGBOOK" "END"))
              ;; Only insert if the drawer doesn't already exist
              (save-excursion
                (org-back-to-heading t)
                (let ((subtree-end (orgist--subtree-end)))
                  (unless (re-search-forward
                           (format "^[ \t]*:%s:[ \t]*$"
                                   (regexp-quote drawer-name))
                           subtree-end t)
                    ;; Insert after properties drawer
                    (org-back-to-heading t)
                    (org-end-of-meta-data)
                    (insert drawer-text "\n")))))))))))

(defun orgist-create-metadata-comment (task-id content)
  "Create a metadata comment on TASK-ID with CONTENT.
Returns the created comment alist, or nil on error."
  (let ((result nil))
    (orgist--request-with-retry
      "https://api.todoist.com/api/v1/comments"
      :type "POST"
      :headers `(("Authorization" . ,(format "Bearer %s" orgist-bearer-token))
                 ("Content-Type" . "application/json"))
      :data (json-encode `((task_id . ,task-id) (content . ,content)))
      :parser 'json-read
      :sync t
      :error (cl-function
              (lambda (&key data error-thrown &allow-other-keys)
                (orgist-log 'warn "Metadata comment create failed for %s: %S (err=%S)"
                            task-id data error-thrown)))
      :success (cl-function
                (lambda (&key data &allow-other-keys)
                  (setq result data))))
    result))

(defun orgist-update-metadata-comment (comment-id content)
  "Update metadata comment COMMENT-ID with CONTENT.
Returns non-nil on success."
  (let ((ok nil))
    (orgist--request-with-retry
      (format "https://api.todoist.com/api/v1/comments/%s" comment-id)
      :type "POST"
      :headers `(("Authorization" . ,(format "Bearer %s" orgist-bearer-token))
                 ("Content-Type" . "application/json"))
      :data (json-encode `((content . ,content)))
      :parser 'json-read
      :sync t
      :error (cl-function
              (lambda (&key data error-thrown &allow-other-keys)
                (orgist-log 'warn "Metadata comment update failed for %s: %S (err=%S)"
                            comment-id data error-thrown)))
      :success (cl-function
                (lambda (&rest _) (setq ok t))))
    ok))

(defun orgist-sync-metadata-comments (commands &optional temp-id-mapping)
  "Create/update metadata comments for tasks modified by COMMANDS.
Only processes tasks that have non-Todoist properties or custom drawers.
TEMP-ID-MAPPING, when non-nil, maps temp IDs to real Todoist IDs
so newly-created tasks (whose buffer ID was remapped) can be found.
Skipped in dry-run mode."
  (when (and orgist-sync-metadata (not orgist-write-back-dry-run))
    (dolist (cmd commands)
      (let* ((args (alist-get 'args cmd))
             (cmd-id (or (alist-get 'id args)
                         (alist-get 'temp_id cmd)))
             ;; After remap, the buffer has the real ID.  Resolve it.
             (id (or (when (and cmd-id temp-id-mapping)
                       (alist-get (intern cmd-id) temp-id-mapping))
                     cmd-id)))
        (when id
          (catch 'found
            (dolist (file (directory-files orgist-base-dir t "\\`[^.].*\\.org\\'"))
              (when-let* ((buf (find-buffer-visiting file)))
                (with-current-buffer buf
                  (when-let* ((pos (orgist-find-element-by-id id)))
                    (save-excursion
                      (goto-char pos)
                      ;; Only process items (not sections)
                      (unless (org-entry-get (point) "SECTION")
                        (let* ((metadata (orgist-extract-metadata))
                               (snap (gethash id orgist-snapshots))
                               (existing-id (when snap
                                              (plist-get snap :metadata-comment-id))))
                          (cond
                           ;; Has metadata + existing comment → update
                           ((and metadata existing-id)
                            (orgist-log 'debug "Updating metadata comment for %s" id)
                            (orgist-update-metadata-comment existing-id metadata))
                           ;; Has metadata + no comment → create
                           ((and metadata (not existing-id))
                            (orgist-log 'debug "Creating metadata comment for %s" id)
                            (let ((result (orgist-create-metadata-comment id metadata)))
                              (when (and result snap)
                                (plist-put snap :metadata-comment-id
                                           (alist-get 'id result)))))
                           ;; No metadata + existing comment → delete
                           ((and (not metadata) existing-id)
                            (orgist-log 'debug "Deleting stale metadata comment for %s" id)
                            (orgist-delete-comment existing-id)
                            (when snap
                              (plist-put snap :metadata-comment-id nil)))))))
                    (throw 'found nil)))))))))))

(defun orgist-sync-task-attachments (task-id comments known-attachments)
  "Sync file attachments for TASK-ID from COMMENTS.
COMMENTS is the list of comment alists (already fetched).
KNOWN-ATTACHMENTS is the snapshot `:attachment-files' alist
of (COMMENT-ID . FILE-NAME) pairs.  Point must be on the heading.

Downloads new attachments into the org-attach directory, removes
attachments whose comments have been deleted.  Returns the updated
attachment-files alist."
  (let* ((known (or known-attachments '()))
         ;; Build set of comment IDs that have attachments
         (current-attachment-comments '())
         (updated known))
    ;; Process each comment for new attachments
    (dolist (comment comments)
      (let* ((comment-id (alist-get 'id comment))
             (attachment (alist-get 'file_attachment comment)))
        (when attachment
          (let ((file-name (alist-get 'file_name attachment))
                (file-url (alist-get 'file_url attachment)))
            (push comment-id current-attachment-comments)
            ;; Download if not already known (keyed by comment-id)
            (when (and file-name file-url
                       (not (assoc comment-id known)))
              (let* ((dir (org-attach-dir-get-create))
                     (existing (expand-file-name file-name dir)))
                (if (file-exists-p existing)
                    ;; File already on disk.  Adopt it only when no other
                    ;; comment already owns it; otherwise this is a second
                    ;; Todoist comment uploading a file with the same name
                    ;; and needs its own disambiguated copy.
                    (if (cl-some (lambda (e) (and (car e) (equal (cdr e) file-name)))
                                 updated)
                        (let* ((unique (orgist--unique-attachment-name dir file-name))
                               (dest (expand-file-name unique dir)))
                          (orgist-log 'info "Downloading duplicate attachment %s as %s for task %s"
                                      file-name unique task-id)
                          (when (orgist-download-file file-url dest)
                            (org-attach-tag)
                            (push (cons comment-id unique) updated)))
                      (orgist-log 'debug "Adopting existing attachment %s for task %s"
                                  file-name task-id)
                      (push (cons comment-id file-name) updated))
                  (let* ((unique (orgist--unique-attachment-name dir file-name))
                         (dest (expand-file-name unique dir)))
                    (orgist-log 'info "Downloading attachment %s for task %s"
                                file-name task-id)
                    (when (orgist-download-file file-url dest)
                      (org-attach-tag)
                      (push (cons comment-id unique) updated))))))))))
    ;; Remove attachments whose comments were deleted
    (let ((to-remove '()))
      (dolist (entry known)
        (let ((comment-id (car entry)))
          (when (and comment-id
                     (not (member comment-id current-attachment-comments))
                     ;; Verify the comment is truly gone (not just missing
                     ;; from a filtered/partial comment list)
                     (not (seq-find (lambda (c) (equal (alist-get 'id c) comment-id))
                                    comments)))
            (push entry to-remove))))
      (dolist (entry to-remove)
        (let* ((file-name (cdr entry))
               (comment-id (car entry))
               (dir (org-attach-dir)))
          (when (and dir (file-exists-p (expand-file-name file-name dir)))
            (orgist-log 'info "Removing stale attachment %s for task %s"
                        file-name task-id)
            (delete-file (expand-file-name file-name dir)))
          (setq updated (assoc-delete-all comment-id updated))))
      ;; Update ATTACH tag based on remaining files
      (let ((dir (org-attach-dir)))
        (if (and dir (file-directory-p dir)
                 (org-attach-file-list dir))
            (org-attach-tag)
          (org-attach-tag 'off))))
    ;; Remove nil-keyed ghost entries whose filename is already covered
    ;; by a real (non-nil) comment-id entry.  These ghosts accumulate when
    ;; orgist-update-snapshots-from-local overwrites the alist with a flat
    ;; filename list and the comment pull later re-adopts real entries on top.
    (let ((real-files (delq nil (mapcar (lambda (e) (and (car e) (cdr e))) updated))))
      (setq updated
            (seq-remove (lambda (entry)
                          (and (null (car entry))
                               (member (cdr entry) real-files)))
                        updated)))
    updated))

(defun orgist--subprocess-comments-pull (&optional force)
  "Pull comments and activity in a subprocess.
Collects non-section task IDs and spawns `emacs --batch' to
fetch comments/activity for each task, then reverts buffers.
When FORCE is nil (auto-pull after sync), only pulls for tasks
that haven't been pulled yet.  When FORCE is non-nil (manual
`orgist-pull-comments'), pulls for all tasks."
  (catch 'orgist-early-return
  (unless orgist-snapshots
    (orgist-load-snapshots))
  ;; Skip if a comments pull is already running (it can take 20+ min).
  ;; Only force-pull (M-x orgist-pull-comments) restarts.
  (when-let* ((old (get-process "orgist-comments")))
    (when (process-live-p old)
      (if force
          (progn
            (orgist-log 'debug "Killing previous comments subprocess")
            (delete-process old))
        (orgist-log 'debug "Comments subprocess already running, skipping")
        (throw 'orgist-early-return nil))))
  (let ((task-ids '()))
    (maphash (lambda (id snap)
               (unless (or (plist-get snap :section-p)
                           (and (not force)
                                (plist-get snap :comments-pulled)))
                 (push id task-ids)))
             orgist-snapshots)
    (setq task-ids (nreverse task-ids))
    (if (null task-ids)
        (orgist-log 'debug "Comments: nothing to pull%s"
                    (if force "" " (all tasks already pulled)"))
    (let* ((ids-file (expand-file-name "comments-task-ids.json" orgist-base-dir))
           (done-file (expand-file-name "comments-done" orgist-base-dir)))
    ;; Write task IDs to temp file
      (with-temp-file ids-file
        (insert (json-encode (vconcat task-ids))))
    (orgist-log 'debug "Spawning comments subprocess for %d tasks" (length task-ids))
    (orgist-log 'debug "Orgist: pulling comments for %d tasks in background..."
                (length task-ids))
    (orgist--run-subprocess
     (list
      :name "orgist-comments"
      :data-files (list ids-file)
      :done-file done-file
      :needs-http t
      :extra-settings `((setq orgist-sync-attachments ,orgist-sync-attachments))
      :open-org-files t
      :job-body `(;; Read task ID list
                  (let* ((json-array-type 'list)
                         (ids (json-read-file ,ids-file))
                         (total (length ids))
                         (count 0)
                         (skipped 0)
                         (errors 0)
                         (start-time (float-time))
                         (last-progress-time start-time))
                    (message "Orgist [INFO] Comments: starting (%d tasks)" total)
                    (dolist (task-id ids)
                      (setq count (1+ count))
                      ;; Progress: every 25 tasks, every 30s, or at the end.
                      ;; Count-based and timer-based checks share one cooldown
                      ;; to avoid near-simultaneous duplicate lines.
                      (let ((now (float-time)))
                        (when (or (<= total 20)
                                  (and (= (% count 25) 0)
                                       (>= (- now last-progress-time) 5.0))
                                  (>= (- now last-progress-time) 30.0)
                                  (= count total))
                          (let* ((elapsed (- now start-time))
                                 (rate (if (> elapsed 0)
                                          (/ (float count) elapsed) 0))
                                 (remaining (if (> rate 0)
                                                (/ (float (- total count)) rate)
                                              0))
                                 (elapsed-m (floor (/ elapsed 60)))
                                 (elapsed-s (floor (mod elapsed 60)))
                                 (eta-m (floor (/ remaining 60)))
                                 (eta-s (floor (mod remaining 60))))
                            (message "Orgist [INFO] Comments: [%d/%d] %dm%02ds elapsed, ~%dm%02ds left (%.1f/min, %d skipped)"
                                     count total elapsed-m elapsed-s
                                     eta-m eta-s (* rate 60) skipped))
                          (setq last-progress-time now)))
                      (condition-case err
                          (let* ((snap (gethash task-id orgist-snapshots))
                                 (nc (when snap (plist-get snap :note-count))))
                            (if (and nc (= nc 0)
                                     (plist-get snap :comments-pulled))
                                (setq skipped (1+ skipped))
                              (orgist-sync-task-comments-and-activity task-id)))
                        (error
                         (setq errors (1+ errors))
                         (let ((name (when-let* ((s (gethash task-id orgist-snapshots)))
                                       (plist-get s :content))))
                           (message "Orgist [WARN] Comments: error on %s%s: %s"
                                    task-id
                                    (if name (format " (%s)" name) "")
                                    (error-message-string err)))))
                      ;; Rate limiting
                      (sleep-for 0.1))
                    ;; Save all buffers and snapshots.
                    ;; Update the visited-file modtime before saving so
                    ;; Emacs doesn't prompt "file has changed since visited"
                    ;; when the main process reverted/saved the file during
                    ;; the subprocess run.
                    (dolist (buf (buffer-list))
                      (when (and (buffer-file-name buf)
                                 (buffer-modified-p buf))
                        (with-current-buffer buf
                          (set-visited-file-modtime)
                          (save-buffer))))
                    (orgist-save-snapshots)
                    (let* ((total-time (- (float-time) start-time))
                           (minutes (floor (/ total-time 60)))
                           (seconds (floor (mod total-time 60))))
                      (with-temp-file ,done-file
                        (insert (format "%d\n" (- total skipped))))
                      (message "Orgist [INFO] Comments: complete — %d tasks in %dm%02ds (%d skipped, %d errors)"
                               total minutes seconds skipped errors))))
      :on-success (lambda (count)
                    (when (and count (> count 0))
                      (message "Orgist: comments: %d tasks synced" count)))
      :on-failure (lambda (event)
                    (message "Orgist: comments sync failed — %s"
                             (string-trim event))))))))))


;;; Project comments

(defun orgist-fetch-project-comments (project-id)
  "Fetch comments for PROJECT-ID from the Todoist API.
Returns a list of comment alists, or nil on error."
  (let ((result nil))
    (request
      (format "https://api.todoist.com/api/v1/comments?project_id=%s" project-id)
      :headers `(("Authorization" . ,(format "Bearer %s" orgist-bearer-token)))
      :parser 'json-read
      :sync t
      :error (cl-function
              (lambda (&key data error-thrown &allow-other-keys)
                (let ((code (orgist--http-error-code error-thrown)))
                  (if (memq code '(403 404 410))
                      (orgist-log 'debug "Project comments: %s returned HTTP %s"
                                  project-id code)
                    (orgist-log 'warn "Project comments API error for %s: %S"
                                project-id data)))))
      :success (cl-function
                (lambda (&key data &allow-other-keys)
                  (let ((comments (if (and (listp data) (assq 'results data))
                                      (alist-get 'results data)
                                    data)))
                    (setq result (append comments nil))))))
    result))

(defun orgist-sync-project-comments (project-id buf)
  "Sync project-level comments for PROJECT-ID into buffer BUF.
Inserts comments as paragraphs between the file header and the
first heading.  Tracks IDs to avoid duplicates."
  (let* ((snapshot-key (concat "proj-comments-" project-id))
         (snapshot (gethash snapshot-key orgist-snapshots))
         (known-ids (when snapshot (plist-get snapshot :project-comment-ids)))
         (comments (orgist-fetch-project-comments project-id))
         (new-ids (copy-sequence (or known-ids '()))))
    (when comments
      (with-current-buffer buf
        (save-excursion
          ;; Find insertion point: after #+keywords and property drawer,
          ;; before first heading
          (goto-char (point-min))
          (let ((insert-point
                 (save-excursion
                   (if (re-search-forward org-outline-regexp-bol nil t)
                       (line-beginning-position)
                     (point-max)))))
            (dolist (comment (append comments nil))
              (let ((comment-id (alist-get 'id comment))
                    (content (alist-get 'content comment)))
                (unless (member comment-id known-ids)
                  (orgist-log 'debug "Project comment: inserting %s for project %s"
                              comment-id project-id)
                  (goto-char insert-point)
                  ;; Skip over any existing blank lines before first heading
                  (skip-chars-backward "\n" (point-min))
                  (when (> (point) (point-min))
                    (forward-char 1))
                  (insert "\n" content "\n")
                  (push comment-id new-ids)))))))
      ;; Update snapshot
      (unless orgist-snapshots
        (setq orgist-snapshots (make-hash-table :test 'equal)))
      (puthash snapshot-key
               (list :project-comment-ids new-ids)
               orgist-snapshots))))

;;;###autoload
(defun orgist-pull-comments ()
  "Manually pull Todoist comments and activity for all tasks.
Forces a full refresh even for previously-pulled tasks.
Runs in a subprocess to avoid blocking the UI."
  (interactive)
  (orgist--subprocess-comments-pull t))

;;; Completed tasks subprocess

(defun orgist--subprocess-completed-tasks-pull (&optional force)
  "Pull completed tasks in a subprocess.
Fetches archived completed tasks from the Todoist API and inserts
them as DONE headings.  When FORCE is non-nil (manual pull),
uses the full lookback window instead of last pull timestamp."
  (catch 'orgist-early-return
  (unless orgist-snapshots
    (orgist-load-snapshots))
  ;; Skip if already running
  (when-let* ((old (get-process "orgist-completed")))
    (when (process-live-p old)
      (if force
          (progn
            (orgist-log 'debug "Killing previous completed-tasks subprocess")
            (delete-process old))
        (orgist-log 'debug "Completed-tasks subprocess already running, skipping")
        (throw 'orgist-early-return nil))))
  ;; Check plan limits
  (unless (orgist-completed-tasks-available-p)
    (orgist-log 'debug "Completed tasks not available on current plan")
    (throw 'orgist-early-return nil))
  ;; Compute time range
  (let* ((until-time (format-time-string "%Y-%m-%dT%H:%M:%SZ" nil t))
         (last-pull (and (not force) (orgist-load-completed-pull-timestamp)))
         (since-time (or last-pull
                         (format-time-string
                          "%Y-%m-%dT%H:%M:%SZ"
                          (time-subtract nil (* orgist-completed-tasks-since-days
                                                86400))
                          t)))
         (config-file (expand-file-name "completed-config.json" orgist-base-dir))
         (done-file (expand-file-name "completed-done" orgist-base-dir)))
    ;; Write config
    (with-temp-file config-file
      (insert (json-encode `((since . ,since-time) (until . ,until-time)))))
    (orgist-log 'debug "Spawning completed-tasks subprocess (since %s)" since-time)
    (orgist-log 'debug "Orgist: pulling completed tasks in background...")
    (orgist--run-subprocess
     (list
      :name "orgist-completed"
      :data-files (list config-file)
      :done-file done-file
      :needs-http t
      :extra-settings nil
      :open-org-files t
      :job-body `(;; Read config
                  (let* ((json-object-type 'alist)
                         (json-key-type 'symbol)
                         (config (json-read-file ,config-file))
                         (since (alist-get 'since config))
                         (until (alist-get 'until config))
                         (start-time (float-time)))
                    ;; Fetch archived sections first so completed
                    ;; tasks can find their parent sections.
                    ;; Only insert sections that don't already exist in
                    ;; their project buffer — re-processing existing ones
                    ;; would mark buffers dirty via org-mode setters even
                    ;; when nothing changed.
                    (let* ((archived-sections (orgist-fetch-archived-sections))
                           (new-sections
                            (seq-filter
                             (lambda (s)
                               (let* ((sid (alist-get 'id s))
                                      (pid (alist-get 'project_id s))
                                      (buf (orgist-get-project-buffer pid)))
                                 (not (and buf
                                           (with-current-buffer buf
                                             (orgist-find-element-by-id sid))))))
                             archived-sections)))
                      (when new-sections
                        (message "Orgist [DEBUG] Inserting %d new archived section(s) (of %d total)"
                                 (length new-sections) (length archived-sections))
                        (orgist-update-elements new-sections 'section)))
                    (message "Orgist [DEBUG] Completed tasks: fetching since %s" since)
                    (let ((tasks (orgist-fetch-completed-tasks since until)))
                      (message "Orgist [DEBUG] Completed tasks: %d tasks fetched"
                               (length tasks))
                      ;; Run even with no new tasks when earlier pulls
                      ;; left unplaced completions awaiting retry.
                      (when (or tasks (file-exists-p (orgist--completed-retry-file)))
                        (orgist-process-completed-tasks tasks))
                      ;; Save buffers and snapshots
                      (dolist (buf (buffer-list))
                        (when (and (buffer-file-name buf)
                                   (buffer-modified-p buf))
                          (with-current-buffer buf (save-buffer))))
                      (orgist-save-snapshots)
                      (orgist-save-completed-pull-timestamp)
                      (let* ((total-time (- (float-time) start-time))
                             (minutes (floor (/ total-time 60)))
                             (seconds (floor (mod total-time 60))))
                        (with-temp-file ,done-file
                          (insert (format "%d\n" (length tasks))))
                        (if (length> tasks 0)
                            (message "Orgist [INFO] Completed tasks: done — %d tasks in %dm%02ds"
                                     (length tasks) minutes seconds)
                          (message "Orgist [DEBUG] Completed tasks: done — 0 tasks in %dm%02ds"
                                   minutes seconds))))))
      :on-success (lambda (count)
                    (when (and count (> count 0))
                      (message "Orgist: completed tasks: %d synced" count))
                    ;; Chain comments pull if enabled (covers both active
                    ;; and newly-inserted completed tasks in one pass)
                    (when orgist-sync-comments
                      (orgist--subprocess-comments-pull)))
      :on-failure (lambda (event)
                    (message "Orgist: completed tasks sync failed — %s"
                             (string-trim event))))))))


;;;###autoload
(defun orgist-pull-completed-tasks ()
  "Manually pull completed tasks from Todoist.
Forces a full refresh using the lookback window."
  (interactive)
  (orgist--subprocess-completed-tasks-pull t))

;;; Quick-add

;;;###autoload
(defun orgist-quick-add (text)
  "Add a task via Todoist's natural language quick-add.
TEXT is a natural language string like \"Buy groceries tomorrow p1 #shopping\".
The task is created via the Todoist API and a sync is triggered to
pull the result into the appropriate org buffer."
  (interactive "sQuick add: ")
  (when (string-empty-p (string-trim text))
    (user-error "Task text cannot be empty"))
  (let ((result nil)
        (token (if (functionp orgist-bearer-token)
                   (funcall orgist-bearer-token)
                 orgist-bearer-token)))
    (request
      "https://api.todoist.com/api/v1/tasks/quick"
      :type "POST"
      :headers `(("Authorization" . ,(format "Bearer %s" token))
                 ("Content-Type" . "application/json"))
      :data (json-encode `((text . ,text)
                           (meta . t)
                           (auto_reminder . t)))
      :parser 'json-read
      :sync t
      :error (cl-function
              (lambda (&key data error-thrown &allow-other-keys)
                (orgist-log 'warn "Quick-add failed: %S (err=%S)" data error-thrown)
                (message "Orgist: quick-add failed — %S" error-thrown)))
      :success (cl-function
                (lambda (&key data &allow-other-keys)
                  (setq result data))))
    (if result
        (progn
          (orgist-log 'debug "Quick-add created task: %s (id=%s)"
                      (alist-get 'content result) (alist-get 'id result))
          (message "Orgist: created \"%s\" — syncing..." (alist-get 'content result))
          ;; Trigger incremental sync to pull the new task
          (when (not orgist-sync-mutex)
            (orgist)))
      (message "Orgist: quick-add failed"))))

;;; Comment push (write-back)

(defun orgist-extract-logbook-notes ()
  "Extract pushable Note entries from the current heading's logbook.
Returns a list of (TIMESTAMP . CONTENT) pairs where TIMESTAMP is
the inactive timestamp string and CONTENT is the note text.

Matches three logbook entry types, all of which may carry user-written text:
  - \"[ts] Note \\\\\"          — standalone note (C-c C-z)
  - \"[ts] Closing Note \\\\\"  — note written when marking DONE
  - \"[ts] From X to Y \\\\\"   — state-change with an attached note

Only entries whose content is non-empty are included."
  (save-excursion
    (org-back-to-heading-or-point-min t)
    (let* ((subtree-end (orgist--subtree-end))
           (start (save-excursion (forward-line 1) (point)))
           (end (save-excursion
                  (org-end-of-meta-data t)
                  (if (re-search-forward org-outline-regexp-bol subtree-end t)
                      (line-beginning-position)
                    subtree-end)))
           (notes '()))
      (goto-char start)
      ;; Match: standalone notes, closing notes (on DONE), and state-change notes.
      ;; The \\\\\\\\n at the end matches the literal " \\" that org appends before
      ;; the note body.
      (while (re-search-forward
              (concat "^[ \t]*- \\(\\[[-0-9]+ [A-Za-z]+ [0-9:]+\\]\\)"
                      " \\(?:Closing Note\\|Note\\|From .* to .*\\) \\\\\\\\\n")
              end t)
        (let ((timestamp (match-string 1))
              (content-lines '()))
          ;; Collect continuation lines (indented by at least 2 spaces)
          (while (and (< (point) end)
                      (looking-at "^  \\(.*\\)\n"))
            (push (match-string 1) content-lines)
            (forward-line 1))
          (let ((content (string-join (nreverse content-lines) "\n")))
            ;; Only include entries that actually have user-written text.
            (unless (string-empty-p (string-trim content))
              (push (cons timestamp content) notes)))))
      (nreverse notes))))

(defun orgist-note-fingerprint (timestamp content)
  "Return a deterministic fingerprint for a Note entry.
TIMESTAMP and CONTENT are strings."
  (sha1 (concat (or timestamp "") (or content ""))))

(defun orgist--fingerprint-p (id)
  "Return non-nil if ID is a SHA1 content fingerprint rather than a Todoist ID.
SHA1 fingerprints are exactly 40 lowercase hex characters."
  (and (stringp id)
       (= (length id) 40)
       (string-match-p "\\`[0-9a-f]+\\'" id)))

(defun orgist--normalize-for-match (s)
  "Normalize S for content matching: collapse whitespace and strip ends."
  (when s
    (string-trim (replace-regexp-in-string "[ \t\n\r]+" " " s))))

(defun orgist--note-content-matches-p (org-content todoist-content)
  "Return non-nil if ORG-CONTENT and TODOIST-CONTENT are substantially the same.
Uses a prefix comparison on normalized text (first 150 chars) to
tolerate minor markdown-to-org conversion differences."
  (let ((n (orgist--normalize-for-match org-content))
        (c (orgist--normalize-for-match todoist-content)))
    (when (and n c (not (string-empty-p n)) (not (string-empty-p c)))
      (let* ((len (min 150 (min (length n) (length c))))
             (n-pre (substring n 0 len))
             (c-pre (substring c 0 len)))
        (string= n-pre c-pre)))))

(defun orgist-repair-comment-ids (&optional dry-run)
  "Scan all tasks for phantom comment IDs and remove them.

A phantom ID is a real Todoist comment ID stored in a task's :comment-ids
snapshot that has no corresponding logbook Note in the org file.  This
happens when the comment subprocess inserted notes but they were later lost
to a concurrent write, or when IDs were recorded despite an insertion error.

For each task with a suspected mismatch (fewer logbook notes than tracked
IDs), fetches the task's Todoist comments and checks whether each comment's
text appears in the logbook.  IDs with no matching note are removed so the
next comment pull can re-insert them.

When DRY-RUN is non-nil, reports findings without modifying any state.
After fixing, saves snapshots and triggers a forced comment pull."
  (unless orgist-snapshots (orgist-load-snapshots))
  ;; Open any unvisited org files so find-element-by-id works.
  (dolist (file (directory-files orgist-base-dir t "\\`[^.].*\\.org\\'"))
    (unless (find-buffer-visiting file)
      (find-file-noselect file t)))
  (let ((total-tasks 0)
        (candidates '())
        (fixed-tasks 0)
        (total-phantoms 0))
    ;; Pass 1 — no API calls: collect tasks where snapshot comment count
    ;; exceeds local logbook note count.
    (maphash
     (lambda (task-id snap)
       (setq total-tasks (1+ total-tasks))
       (unless (plist-get snap :section-p)
         (let* ((comment-ids (plist-get snap :comment-ids))
                (meta-id (plist-get snap :metadata-comment-id))
                ;; Real Todoist IDs: exclude content fingerprints and the
                ;; metadata comment which is never inserted as a Note.
                (real-ids (seq-filter
                           (lambda (id)
                             (and id
                                  (not (equal id meta-id))
                                  (not (orgist--fingerprint-p id))))
                           (or comment-ids '())))
                (real-count (length real-ids)))
           (when (> real-count 0)
             ;; Count logbook notes actually present in org.
             (catch 'found
               (dolist (buf (buffer-list))
                 (when (and (buffer-file-name buf)
                            (string-match-p "\\.org\\'" (buffer-file-name buf)))
                   (with-current-buffer buf
                     (when-let* ((pos (orgist-find-element-by-id task-id)))
                       (save-excursion
                         (goto-char pos)
                         (org-back-to-heading t)
                         (let ((note-count (length (orgist-extract-logbook-notes))))
                           (when (< note-count real-count)
                             (push (list task-id snap real-ids) candidates))))
                       (throw 'found nil))))))))))
     orgist-snapshots)
    (orgist-log 'info "Repair: %d candidate task(s) with potential phantom IDs"
                (length candidates))
    ;; Pass 2 — API calls: for each candidate, fetch Todoist comments and
    ;; do content-based matching to confirm which IDs are phantom.
    (dolist (candidate candidates)
      (let* ((task-id (nth 0 candidate))
             (snap    (nth 1 candidate))
             (real-ids (nth 2 candidate))
             (todoist-comments (orgist-fetch-task-comments task-id))
             (logbook-notes '())
             (phantom-ids '()))
        ;; Collect current logbook notes for this task.
        (catch 'found
          (dolist (buf (buffer-list))
            (when (and (buffer-file-name buf)
                       (string-match-p "\\.org\\'" (buffer-file-name buf)))
              (with-current-buffer buf
                (when-let* ((pos (orgist-find-element-by-id task-id)))
                  (save-excursion
                    (goto-char pos)
                    (org-back-to-heading t)
                    (setq logbook-notes (orgist-extract-logbook-notes)))
                  (throw 'found nil))))))
        ;; For each real Todoist ID in the snapshot, check whether the
        ;; comment's text appears as a logbook note.
        (dolist (cid real-ids)
          (let ((comment (seq-find (lambda (c) (equal (alist-get 'id c) cid))
                                   todoist-comments)))
            (cond
             ((null comment)
              ;; Comment no longer exists in Todoist — keep the ID as-is
              ;; rather than triggering a re-pull that would fail.
              (orgist-log 'debug "Repair: comment %s not in Todoist for %s (skipping)"
                          cid task-id))
             (t
              (let* ((raw-content (alist-get 'content comment))
                     (converted (condition-case nil
                                    (orgist-convert-description raw-content 1)
                                  (error raw-content)))
                     (matched (seq-some
                               (lambda (note)
                                 (orgist--note-content-matches-p (cdr note) converted))
                               logbook-notes)))
                (unless matched
                  (push cid phantom-ids)))))))
        (when phantom-ids
          (setq fixed-tasks (1+ fixed-tasks))
          (setq total-phantoms (+ total-phantoms (length phantom-ids)))
          (orgist-log 'info "Repair: %d phantom ID(s) in \"%s\" [%s]%s"
                      (length phantom-ids)
                      (or (plist-get snap :content) "?")
                      task-id
                      (if dry-run " [DRY RUN]" ""))
          (unless dry-run
            (let* ((updated (copy-sequence snap))
                   (remaining (seq-remove (lambda (id) (member id phantom-ids))
                                          (plist-get snap :comment-ids))))
              (plist-put updated :comment-ids remaining)
              ;; Allow comment pull to re-process this task.
              (plist-put updated :comments-pulled nil)
              (puthash task-id updated orgist-snapshots))))
        ;; Respect rate limits between API calls.
        (sleep-for 0.1)))
    (orgist-log 'info "Repair: %d task(s) fixed, %d phantom ID(s) cleared%s"
                fixed-tasks total-phantoms
                (if dry-run " [DRY RUN — no changes written]" ""))
    (unless dry-run
      (when (> fixed-tasks 0)
        (orgist-save-snapshots)
        (orgist-log 'info "Repair: triggering forced comment pull to re-insert missing notes")
        (orgist--subprocess-comments-pull t)))
    (list :candidates (length candidates)
          :fixed fixed-tasks
          :phantoms total-phantoms)))

;;; Footer
(provide 'orgist)

;;; orgist.el ends here
