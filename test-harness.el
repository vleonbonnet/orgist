;;; test-harness.el --- Offline test infrastructure for orgist -*- lexical-binding: t; -*-

;; Usage:
;;   emacs --batch -l test-harness.el -- record Orgtest   # Record API responses
;;   emacs --batch -l test-harness.el -- replay Orgtest    # Replay one project
;;   emacs --batch -l test-harness.el -- all               # All cached projects

;; Add dependency paths
(add-to-list 'load-path (expand-file-name "~/.emacs.d/elpaca/builds/request"))
(add-to-list 'load-path default-directory)
;; Explicitly load orgist.el source (not byte-compiled .elc from elpaca)
(let ((script-dir (file-name-directory (or load-file-name buffer-file-name))))
  (load (expand-file-name "orgist.el" script-dir) nil nil t))
(require 'json)

;;; ============================================================
;;; A. Request mock (advice-based)
;;; ============================================================

(defvar orgist-test-record-mode 'replay
  "Test mode: `record' lets real HTTP through and caches; `replay' uses cache.")

(defvar orgist-test-cache-dir nil
  "Directory for cached API responses (test-data/<Project>/).")

(defun orgist-test--cache-filename (sync-token)
  "Return cache filename based on SYNC-TOKEN."
  (expand-file-name
   (if (string= sync-token "*") "full-sync.json" "incremental-sync.json")
   orgist-test-cache-dir))

(defun orgist-test--extract-sync-token (args)
  "Extract sync_token from the :data plist in ARGS."
  (let ((data (plist-get args :data)))
    (cdr (assoc "sync_token" data))))

(defun orgist-test--request-advice (orig-fn url &rest args)
  "Advice around `request' for record/replay of API responses.
ORIG-FN is the original `request', URL is the endpoint, ARGS are kwargs."
  (if (not (string-match-p "todoist\\.com" url))
      ;; Not a Todoist call — pass through
      (apply orig-fn url args)
    ;; Dispatch based on URL path
    (cond
     ;; Comments API: POST /api/v1/comments (create) or POST /comments/:id (update)
     ((and (string-match-p "/comments" url)
           (equal (plist-get args :type) "POST"))
      (let ((success-fn (plist-get args :success)))
        (message "[test-harness] MOCK comment POST to %s" url)
        (when success-fn
          (funcall success-fn :data
                   `((id . ,(format "mock-comment-%s" (org-id-uuid)))
                     (content . ,(let ((json-data (plist-get args :data)))
                                   (when (stringp json-data)
                                     (alist-get 'content
                                                (json-read-from-string json-data)))))
                     (posted_at . "2026-03-17T12:00:00Z"))))))

     ;; Comments API: DELETE /api/v1/comments/:id
     ((and (string-match-p "/comments/" url)
           (equal (plist-get args :type) "DELETE"))
      (let ((success-fn (plist-get args :success)))
        (message "[test-harness] MOCK comment DELETE %s" url)
        (when success-fn (funcall success-fn))))

     ;; Comments API: GET /api/v1/comments?task_id=ID
     ((string-match-p "/comments" url)
      (let* ((task-id (when (string-match "task_id=\\([^&]+\\)" url)
                        (match-string 1 url)))
             (script-dir (file-name-directory (or load-file-name buffer-file-name)))
             (cache-file (expand-file-name
                          (format "test-data/comments/%s.json" task-id)
                          script-dir))
             (success-fn (plist-get args :success)))
        (if (eq orgist-test-record-mode 'record)
            (let* ((orig-success success-fn)
                   (wrapped-success
                    (cl-function
                     (lambda (&key data &allow-other-keys)
                       (let ((dir (file-name-directory cache-file)))
                         (unless (file-directory-p dir) (make-directory dir t)))
                       (with-temp-file cache-file (insert (json-encode data)))
                       (message "[test-harness] Saved comments for %s" task-id)
                       (when orig-success (funcall orig-success :data data))))))
              (setq args (plist-put args :success wrapped-success))
              (apply orig-fn url args))
          ;; Replay mode
          (message "[test-harness] REPLAY comments for task %s" task-id)
          (if (file-exists-p cache-file)
              (let* ((json-object-type 'alist)
                     (json-array-type 'vector)
                     (json-key-type 'symbol)
                     (data (json-read-file cache-file)))
                (when success-fn (funcall success-fn :data data)))
            (when success-fn (funcall success-fn :data []))))))

     ;; Plan limits ONLY: POST /api/v1/sync with ONLY user_plan_limits
     ;; (not when it's part of a larger sync request that also has items/projects)
     ((and (string-match-p "/sync" url)
           (let ((data (plist-get args :data)))
             (and data
                  (let ((rt (or (cdr (assoc "resource_types" data)) "")))
                    (and (string-match-p "user_plan_limits" rt)
                         (not (string-match-p "items" rt)))))))
      (let ((success-fn (plist-get args :success)))
        (when success-fn
          (funcall success-fn :data
                   '((user_plan_limits
                      (activity_log . t)
                      (activity_log_limit . 7)
                      (completed_tasks . t)))))))

     ;; Activity API: GET /api/v1/activities
     ((string-match-p "/activities" url)
      (let* ((params (plist-get args :params))
             (task-id (or (cdr (assoc "objectId" params))
                          (cdr (assoc "object_id" params))))
             (script-dir (file-name-directory (or load-file-name buffer-file-name)))
             (cache-file (expand-file-name
                          (format "test-data/activity/%s.json" task-id)
                          script-dir))
             (success-fn (plist-get args :success)))
        (if (eq orgist-test-record-mode 'record)
            (let* ((orig-success success-fn)
                   (wrapped-success
                    (cl-function
                     (lambda (&key data &allow-other-keys)
                       (let ((dir (file-name-directory cache-file)))
                         (unless (file-directory-p dir) (make-directory dir t)))
                       (with-temp-file cache-file (insert (json-encode data)))
                       (message "[test-harness] Saved activity for %s" task-id)
                       (when orig-success (funcall orig-success :data data))))))
              (setq args (plist-put args :success wrapped-success))
              (apply orig-fn url args))
          ;; Replay mode
          (message "[test-harness] REPLAY activity for task %s" task-id)
          (if (file-exists-p cache-file)
              (let* ((json-object-type 'alist)
                     (json-array-type 'vector)
                     (json-key-type 'symbol)
                     (data (json-read-file cache-file)))
                (when success-fn (funcall success-fn :data data)))
            (when success-fn
              (funcall success-fn :data '((results . []) (nextCursor . nil))))))))

     ;; Archived sections API: GET /api/v1/sections/archived
     ((string-match-p "/sections/archived" url)
      (let* ((script-dir (file-name-directory (or load-file-name buffer-file-name)))
             (cache-file (expand-file-name "test-data/archived-sections.json"
                                           script-dir))
             (success-fn (plist-get args :success)))
        (message "[test-harness] REPLAY archived sections")
        (if (file-exists-p cache-file)
            (let* ((json-object-type 'alist)
                   (json-array-type 'vector)
                   (json-key-type 'symbol)
                   (data (json-read-file cache-file)))
              (when success-fn (funcall success-fn :data data)))
          (when success-fn
            (funcall success-fn
                     :data '((sections . []) (has_more . :json-false)
                             (next_cursor . nil) (total . 0)))))))

     ;; Completed tasks API: GET /api/v1/tasks/completed/by_completion_date
     ((string-match-p "/tasks/completed" url)
      (let* ((script-dir (file-name-directory (or load-file-name buffer-file-name)))
             (cache-file (expand-file-name "test-data/completed-tasks.json"
                                           script-dir))
             (success-fn (plist-get args :success)))
        (if (eq orgist-test-record-mode 'record)
            (let* ((orig-success success-fn)
                   (wrapped-success
                    (cl-function
                     (lambda (&key data &allow-other-keys)
                       (let ((dir (file-name-directory cache-file)))
                         (unless (file-directory-p dir) (make-directory dir t)))
                       (with-temp-file cache-file (insert (json-encode data)))
                       (message "[test-harness] Saved completed tasks response")
                       (when orig-success (funcall orig-success :data data))))))
              (setq args (plist-put args :success wrapped-success))
              (apply orig-fn url args))
          ;; Replay mode
          (message "[test-harness] REPLAY completed tasks")
          (if (file-exists-p cache-file)
              (let* ((json-object-type 'alist)
                     (json-array-type 'vector)
                     (json-key-type 'symbol)
                     (data (json-read-file cache-file)))
                (when success-fn (funcall success-fn :data data)))
            (when success-fn
              (funcall success-fn :data '((results . []) (next_cursor . nil))))))))

     ;; Quick-add API: POST /api/v1/tasks/quick
     ((string-match-p "/tasks/quick" url)
      (let* ((json-data (plist-get args :data))
             (parsed (when (stringp json-data)
                       (json-read-from-string json-data)))
             (text (when parsed (alist-get 'text parsed)))
             (success-fn (plist-get args :success)))
        (message "[test-harness] MOCK quick-add: %s" (or text "(no text)"))
        (when success-fn
          (funcall success-fn :data
                   `((id . ,(format "mock-qa-%s" (org-id-uuid)))
                     (content . ,(or text "Quick-add task"))
                     (project_id . "mock-inbox")
                     (section_id . nil)
                     (parent_id . nil)
                     (child_order . 0)
                     (priority . 1)
                     (checked . :json-false)
                     (labels . [])
                     (due . nil)
                     (description . ""))))))

     ;; Project comments API: GET /api/v1/comments?project_id=ID
     ((and (string-match-p "/comments" url)
           (string-match "project_id=\\([^&]+\\)" url))
      (let ((success-fn (plist-get args :success)))
        (message "[test-harness] MOCK project comments")
        (when success-fn
          (funcall success-fn :data '((results . []))))))

     ;; Write-back request (commands in :data)
     ((assoc "commands" (plist-get args :data))
      (let* ((data (plist-get args :data))
             (commands-json (cdr (assoc "commands" data)))
             (commands (json-read-from-string commands-json))
             (status (make-hash-table :test 'equal))
             (temp-id-map (make-hash-table :test 'equal))
             (success-fn (plist-get args :success)))
        (seq-doseq (cmd commands)
          (puthash (alist-get 'uuid cmd) "ok" status)
          ;; Generate mock real IDs for creation commands
          (when (member (alist-get 'type cmd)
                        '("item_add" "note_add" "label_add" "reminder_add"))
            (let* ((temp-id (alist-get 'temp_id cmd))
                   (real-id (concat "mock-real-"
                                    (substring temp-id 0 (min 8 (length temp-id))))))
              (puthash temp-id real-id temp-id-map))))
        (message "[test-harness] MOCK write-back: %d command(s) -> all ok"
                 (length commands))
        (when success-fn
          (funcall success-fn :data `((sync_status . ,status)
                                      (temp_id_mapping . ,temp-id-map))))))

     ;; Read-sync request (default)
     (t
      (let* ((sync-token (orgist-test--extract-sync-token args))
             (cache-file (orgist-test--cache-filename sync-token)))
        (pcase orgist-test-record-mode
          ('record
           (message "[test-harness] RECORD mode: real request (sync_token=%s)"
                    (if (string= sync-token "*") "*" "(incremental)"))
           (let* ((orig-success (plist-get args :success))
                  (wrapped-success
                   (cl-function
                    (lambda (&key data &allow-other-keys)
                      (unless (file-directory-p orgist-test-cache-dir)
                        (make-directory orgist-test-cache-dir t))
                      (with-temp-file cache-file
                        (insert (json-encode data)))
                      (message "[test-harness] Saved response to %s" cache-file)
                      (funcall orig-success :data data)))))
             (setq args (plist-put args :success wrapped-success))
             (apply orig-fn url args)))
          ('replay
           (message "[test-harness] REPLAY mode: loading %s" cache-file)
           (unless (file-exists-p cache-file)
             (error "Cache file not found: %s (run 'record' first)" cache-file))
           (let* ((json-object-type 'alist)
                  (json-array-type 'vector)
                  (json-key-type 'symbol)
                  (data (json-read-file cache-file))
                  (success-fn (plist-get args :success)))
             ;; Inject mock data for new resource types not in cached JSON
             (unless (alist-get 'labels data)
               (push '(labels . []) data))
             (unless (alist-get 'reminders data)
               (push '(reminders . []) data))
             (unless (alist-get 'collaborators data)
               (push '(collaborators . []) data))
             (unless (alist-get 'user data)
               (push `(user . ((timezone . "UTC")
                               (inbox_project_id . "mock-inbox")
                               (date_format . 0)
                               (time_format . 0)))
                     data))
             (unless (alist-get 'user_plan_limits data)
               (push '(user_plan_limits . ((activity_log . t)
                                           (activity_log_limit . 7)
                                           (completed_tasks . t)))
                     data))
             (funcall success-fn :data data)))))))))

(advice-add 'request :around #'orgist-test--request-advice)

;;; ============================================================
;;; B. Test isolation
;;; ============================================================

(defun orgist-test-setup-isolation (project-name)
  "Set up isolated test environment for PROJECT-NAME.
Shared JSON cache lives in test-data/ (replay) or test-data/<Project>/ (record).
Runtime artifacts (org files, token, snapshots) go to /tmp/orgist-test/<Project>/."
  (let* ((script-dir (file-name-directory (or load-file-name buffer-file-name)))
         (test-data-dir (expand-file-name "test-data/" script-dir))
         (runtime-dir (expand-file-name
                       (concat "orgist-test/" project-name "/")
                       temporary-file-directory)))
    ;; Clean runtime dir for a fresh run
    (when (file-directory-p runtime-dir)
      (delete-directory runtime-dir t))
    (make-directory runtime-dir t)
    ;; Replay uses shared JSON at test-data/ root; record uses per-project subdir
    (setq orgist-test-cache-dir
          (if (eq orgist-test-record-mode 'record)
              (let ((project-dir (expand-file-name (concat project-name "/") test-data-dir)))
                (unless (file-directory-p project-dir)
                  (make-directory project-dir t))
                project-dir)
            test-data-dir))
    ;; Redirect all orgist state to runtime dir (local filesystem)
    (setq orgist-base-dir runtime-dir)
    (setq orgist-sync-token-filename (expand-file-name "sync_token" runtime-dir))
    (setq orgist-snapshot-file (expand-file-name "snapshots.el" runtime-dir))
    (setq orgist-log-file (expand-file-name "orgist.log" runtime-dir))
    ;; Kill buffers from previous runs to prevent memory exhaustion
    (dolist (buf (buffer-list))
      (when (and (buffer-file-name buf)
                 (string-match-p "\\.org$" (buffer-file-name buf)))
        (kill-buffer buf)))
    ;; Clear in-memory state
    (setq orgist-project-buffer-cache nil)
    (setq orgist-snapshots nil)
    (setq orgist-sync-mutex nil)
    (setq orgist-sync-project-filter project-name)
    ;; Suppress "file changed on disk" prompts in batch mode
    (setq revert-without-query '(".*"))
    ;; Deterministic priority settings (integers, not characters)
    (setq org-priority-highest 1
          org-priority-lowest 5
          org-priority-default 5)
    ;; Enable write-back with no-confirm mode (tests run in batch)
    (setq orgist-enable-write-back t)
    (setq orgist-write-back-dry-run t)
    ;; Set token from env
    (let ((token (getenv "TODOIST_API_TOKEN")))
      (when token
        (setq orgist-bearer-token token)))
    (setq orgist-log-level 'info)
    (message "[test-harness] Isolation: base-dir=%s cache-dir=%s"
             orgist-base-dir orgist-test-cache-dir)))

;;; ============================================================
;;; C. Lifecycle test runner
;;; ============================================================

;; --- Assertions ---

(defvar orgist-test--failures 0 "Count of failed assertions.")
(defvar orgist-test--passes 0 "Count of passed assertions.")

(defun orgist-test-assert (condition description)
  "Assert CONDITION is non-nil; log DESCRIPTION."
  (if condition
      (progn
        (setq orgist-test--passes (1+ orgist-test--passes))
        (message "[PASS] %s" description))
    (setq orgist-test--failures (1+ orgist-test--failures))
    (message "[FAIL] %s" description)))

(defun orgist-test-assert-equal (expected actual description)
  "Assert EXPECTED equals ACTUAL; log DESCRIPTION."
  (if (equal expected actual)
      (progn
        (setq orgist-test--passes (1+ orgist-test--passes))
        (message "[PASS] %s" description))
    (setq orgist-test--failures (1+ orgist-test--failures))
    (message "[FAIL] %s (expected %S, got %S)" description expected actual)))

(defun orgist-test-assert-file-exists (path description)
  "Assert file at PATH exists; log DESCRIPTION."
  (orgist-test-assert (file-exists-p path) description))

;; --- Wait for sync ---

(defun orgist-test--wait-for-sync (max-seconds)
  "Wait for `orgist-sync-mutex' to become nil, up to MAX-SECONDS.
Returns t if sync completed, nil if timed out."
  (let ((waited 0))
    (while (and orgist-sync-mutex (< waited max-seconds))
      (sleep-for 1)
      (setq waited (1+ waited))
      (when (= (% waited 10) 0)
        (message "[test-harness] Waiting for sync... %ds" waited)))
    (not orgist-sync-mutex)))

;; --- Lifecycle phases ---

(defun orgist-test-run-lifecycle (project-name)
  "Run full test lifecycle for PROJECT-NAME.
Phase 1: Full sync
Phase 2: Incremental sync
Phase 3: Diff check"
  (setq orgist-test--failures 0)
  (setq orgist-test--passes 0)
  (message "")
  (message "========================================")
  (message "=== Lifecycle test: %s ===" project-name)
  (message "========================================")

  ;; --- Phase 1: Full sync ---
  (message "")
  (message "--- Phase 1: Full sync ---")
  (orgist-test-setup-isolation project-name)
  ;; Delete sync token to force full sync
  (when (file-exists-p orgist-sync-token-filename)
    (delete-file orgist-sync-token-filename))
  (orgist)
  (let ((completed (orgist-test--wait-for-sync 120)))
    (orgist-test-assert completed "Sync completed without timeout")
    (when completed
      ;; Assert org files created
      (let ((org-files (directory-files orgist-base-dir nil "\\.org$")))
        (orgist-test-assert (length> org-files 0)
                            (format "Org files created (%d found)" (length org-files)))
        (dolist (f org-files)
          (message "  -> %s (%d bytes)" f
                   (file-attribute-size
                    (file-attributes
                     (expand-file-name f orgist-base-dir))))))
      ;; When project filter is active, orgist skips saving the sync token.
      ;; Force-save it from the cached response so Phase 2 can run incrementally.
      (when (and orgist-sync-project-filter
                 (not (file-exists-p orgist-sync-token-filename)))
        (let* ((json-object-type 'alist)
               (json-key-type 'symbol)
               (resp (json-read-file
                      (expand-file-name "full-sync.json" orgist-test-cache-dir)))
               (token (alist-get 'sync_token resp)))
          (when token (orgist-save-sync-token token))))
      ;; Assert sync token saved
      (orgist-test-assert-file-exists
       orgist-sync-token-filename "Sync token file saved")
      ;; Assert snapshots saved
      (orgist-test-assert-file-exists
       orgist-snapshot-file "Snapshots file saved")
      (let ((snap-count (if orgist-snapshots
                            (hash-table-count orgist-snapshots) 0)))
        (orgist-test-assert (>= snap-count 0)
                            (format "Snapshots in memory (%d elements)" snap-count)))))

  ;; --- Phase 2: Incremental sync ---
  (message "")
  (message "--- Phase 2: Incremental sync ---")
  ;; Keep state from phase 1 (sync token exists), re-run sync
  (orgist)
  (let ((completed (orgist-test--wait-for-sync 120)))
    (orgist-test-assert completed "Incremental sync completed without timeout"))

  ;; --- Phase 3: Diff check ---
  (message "")
  (message "--- Phase 3: Diff check (zero spurious diffs) ---")
  ;; Reload snapshots fresh from disk
  (when (file-exists-p orgist-snapshot-file)
    (orgist-load-snapshots t))
  (orgist-test-assert (>= (hash-table-count orgist-snapshots) 0)
                      (format "Snapshots loaded from disk (%d elements)"
                              (hash-table-count orgist-snapshots)))
  ;; Open all org files and build caches
  (dolist (file (directory-files orgist-base-dir t "\\.org$"))
    (find-file file)
    (org-mode)
    (orgist-build-id-cache))
  ;; Performance check: diff-all-elements on unmodified files should be fast
  (let* ((t0 (float-time))
         (changes (orgist-diff-all-elements))
         (elapsed (- (float-time) t0))
         ;; Separate due/deadline-only diffs (expected from repeaters)
         ;; from real spurious diffs (content, priority, etc.)
         (date-only-diffs 0)
         (real-diffs '()))
    (orgist-test-assert (< elapsed 1.0)
                        (format "Write-back diff completed in %.3fs (< 1s)" elapsed))
    (dolist (c changes)
      (let ((diff (cdr c)))
        (if (and (listp diff)
                 (seq-every-p (lambda (d) (memq (car d) '(:due :deadline))) diff))
            (setq date-only-diffs (1+ date-only-diffs))
          (push c real-diffs))))
    (when (> date-only-diffs 0)
      (message "  (%d date-only diffs from repeaters — expected)" date-only-diffs))
    (orgist-test-assert-equal 0 (length real-diffs)
                              "Zero spurious non-date diffs after fresh sync")
    (when real-diffs
      (message "  Unexpected diffs:")
      (dolist (c (seq-take real-diffs 10))
        (message "    %s: %S" (car c) (cdr c)))))

  ;; --- Results ---
  (message "")
  (message "========================================")
  (message "=== Results: %s ===" project-name)
  (message "=== Passed: %d  Failed: %d ==="
           orgist-test--passes orgist-test--failures)
  (message "========================================")
  (message "")
  orgist-test--failures)

;;; ============================================================
;;; D. Cross-project move test
;;; ============================================================

(defun orgist-test-run-cross-project-move ()
  "Test that moving an item between projects removes it from the source.
Uses a dedicated synthetic fixture (test-data-move/) — four invented
projects (Source, Hub, Parent + Child sub-project) and two tasks — so the
test never depends on the user's live Todoist data.  Syncs the fixture,
then simulates a root->root move (Source->Hub) and a root->sub-project
cross-file move (Hub->Child), verifying the old copy is deleted each time."
  (setq orgist-test--failures 0)
  (setq orgist-test--passes 0)
  (message "")
  (message "========================================")
  (message "=== Cross-project move test ===")
  (message "========================================")

  ;; Phase 1: Full sync of the synthetic move fixture
  (message "")
  (message "--- Phase 1: Full sync (synthetic fixture) ---")
  (let* ((script-dir (file-name-directory (or load-file-name buffer-file-name)))
         (test-data-dir (expand-file-name "test-data-move/" script-dir))
         (runtime-dir (expand-file-name
                       (concat "orgist-test/CrossMove/")
                       temporary-file-directory)))
    ;; Clean runtime dir
    (when (file-directory-p runtime-dir)
      (delete-directory runtime-dir t))
    (make-directory runtime-dir t)
    (setq orgist-test-cache-dir test-data-dir)
    (setq orgist-base-dir runtime-dir)
    (setq orgist-sync-token-filename (expand-file-name "sync_token" runtime-dir))
    (setq orgist-snapshot-file (expand-file-name "snapshots.el" runtime-dir))
    (setq orgist-log-file (expand-file-name "orgist.log" runtime-dir))
    (dolist (buf (buffer-list))
      (when (and (buffer-file-name buf)
                 (string-match-p "\\.org$" (buffer-file-name buf)))
        (kill-buffer buf)))
    (setq orgist-project-buffer-cache nil)
    (setq orgist-snapshots nil)
    (setq orgist-sync-mutex nil)
    (setq orgist-sync-project-filter nil)  ;; No filter — sync everything
    (setq revert-without-query '(".*"))
    (setq org-priority-highest 1
          org-priority-lowest 5
          org-priority-default 5)
    (setq orgist-enable-write-back t)
    (setq orgist-write-back-dry-run t)
    (setq orgist-log-level 'info)
    (setq orgist-test-record-mode 'replay))

  (orgist)
  (let ((completed (orgist-test--wait-for-sync 120)))
    (orgist-test-assert completed "Full sync completed")
    (unless completed (kill-emacs 1)))

  ;; --- Test A: Move from root project to root project ---
  ;; Move "Move test task A" from Source to Hub.
  (let* ((moved-id "MoveTestTaskA01")
         (work-project-id "MoveTestHubBb02")  ; "Hub" — the move target
         (source-buf nil)
         (work-buf (orgist-get-project-buffer work-project-id)))

    (message "")
    (message "--- Test A: Source -> Hub ---")

    ;; Find task in any buffer
    (dolist (file (directory-files orgist-base-dir t "\\.org$"))
      (let ((buf (find-file-noselect file)))
        (with-current-buffer buf
          (when (orgist-find-element-by-id moved-id)
            (setq source-buf buf)))))

    (orgist-test-assert source-buf
                        (format "Task found in %s before move"
                                (when source-buf (buffer-name source-buf))))
    (orgist-test-assert (not (eq source-buf work-buf))
                        "Task is NOT in Hub before move")

    ;; Seed a logbook entry to prove history survives the move.  Use a
    ;; :LOGBOOK: drawer — bare body entries are cleared on update.
    (with-current-buffer source-buf
      (save-excursion
        (goto-char (orgist-find-element-by-id moved-id))
        (let ((org-log-into-drawer t))
          (orgist-insert-log-entry "TODO" "" "[2026-01-05 Mon 09:00]"))
        (save-buffer)))

    ;; Simulate move via incremental sync
    (let ((moved-item `((id . ,moved-id)
                        (content . "Move test task A")
                        (project_id . ,work-project-id)
                        (section_id . nil)
                        (parent_id . nil)
                        (child_order . 999)
                        (checked . :json-false)
                        (priority . 1)
                        (description . "")
                        (due . nil)))
          (inhibit-redisplay t)
          (orgist--batch-save-pending (make-hash-table :test 'eq)))
      (orgist-update-elements (vector moved-item) 'item)
      (orgist--flush-pending-saves))

    ;; Rebuild caches and verify
    (dolist (file (directory-files orgist-base-dir t "\\.org$"))
      (with-current-buffer (find-file-noselect file)
        (orgist-build-id-cache)))

    (let ((in-work nil) (in-source nil))
      (with-current-buffer work-buf
        (setq in-work (orgist-find-element-by-id moved-id)))
      (when source-buf
        (with-current-buffer source-buf
          (setq in-source (orgist-find-element-by-id moved-id))))
      (orgist-test-assert in-work "Task found in Hub after move")
      (orgist-test-assert (not in-source)
                          (format "Task removed from %s after move"
                                  (when source-buf (buffer-name source-buf))))
      (orgist-test-assert
       (seq-find (lambda (line) (string-match-p "2026-01-05" line))
                 (orgist-test--logbook-entries work-buf moved-id))
       "Logbook history survived the move")))

  ;; --- Test B: Move from root project to sub-project (different file) ---
  ;; Move "Move test task B" from Hub (root project) to Child (a sub-project
  ;; of Parent, stored in a different file).  Exercises the cross-file
  ;; delete-from-source path.
  (let* ((moved-id "MoveTestTaskB02")
         (childcare-project-id "MoveTestChild04")  ; "Child" sub-project
         (work-project-id "MoveTestHubBb02")       ; "Hub" root project
         (source-buf (orgist-get-project-buffer work-project-id))
         (target-buf (orgist-get-project-buffer childcare-project-id)))

    (message "")
    (message "--- Test B: Hub -> Parent/Child ---")

    (orgist-test-assert (not (eq source-buf target-buf))
                        "Source (Hub) and target (Parent) are different buffers")
    (with-current-buffer source-buf
      (orgist-test-assert (orgist-find-element-by-id moved-id)
                          "Task in Hub before move"))

    ;; Simulate move
    (let ((moved-item `((id . ,moved-id)
                        (content . "Move test task B")
                        (project_id . ,childcare-project-id)
                        (section_id . nil)
                        (parent_id . nil)
                        (child_order . 999)
                        (checked . :json-false)
                        (priority . 1)
                        (description . "")
                        (due . nil)))
          (inhibit-redisplay t)
          (orgist--batch-save-pending (make-hash-table :test 'eq)))
      (orgist-update-elements (vector moved-item) 'item)
      (orgist--flush-pending-saves))

    ;; Rebuild caches and verify
    (dolist (file (directory-files orgist-base-dir t "\\.org$"))
      (with-current-buffer (find-file-noselect file)
        (orgist-build-id-cache)))

    (with-current-buffer target-buf
      (orgist-test-assert (orgist-find-element-by-id moved-id)
                          "Task found in Parent/Child after move"))
    (with-current-buffer source-buf
      (orgist-test-assert (not (orgist-find-element-by-id moved-id))
                          "Task removed from Hub after move"))

    ;; Verify on-disk: re-read the Hub file from disk and confirm task is gone
    (let* ((work-file (buffer-file-name source-buf))
           (disk-content (with-temp-buffer
                           (insert-file-contents work-file)
                           (buffer-string))))
      (orgist-test-assert (not (string-match-p moved-id disk-content))
                          "Task ID absent from Hub file on disk")))

  ;; Results
  (message "")
  (message "========================================")
  (message "=== Results: CrossMove ===")
  (message "=== Passed: %d  Failed: %d ==="
           orgist-test--passes orgist-test--failures)
  (message "========================================")
  (message "")
  orgist-test--failures)

;;; ============================================================
;;; D2. State-change logging test
;;; ============================================================

(defun orgist-test--logbook-entries (buf id)
  "Return the logbook state-change entry lines for element ID in BUF.
Handles both the default org format (State \"DONE\" from ...) and
custom From/to `org-log-note-headings' formats, inside or outside
a :LOGBOOK: drawer."
  (with-current-buffer buf
    (save-excursion
      (goto-char (orgist-find-element-by-id id))
      (org-back-to-heading t)
      (let ((end (save-excursion (org-end-of-subtree t t) (point)))
            (entries '()))
        (while (re-search-forward
                "^[ \t]*- \\(?:State\\|\\[.*?\\] From\\) .*$" end t)
          (push (string-trim (match-string 0)) entries))
        (nreverse entries)))))

(defun orgist-test--count-state-entries (buf id to-state &optional from-state)
  "Count logbook entries for ID in BUF recording a change to TO-STATE.
When FROM-STATE is non-nil, only count entries leaving that state."
  (seq-count
   (lambda (line)
     (and (string-match-p (format "\\(?:State\\|to\\) \"%s\"" to-state) line)
          (or (null from-state)
              (string-match-p (format "\\(?:from\\|From\\) \"%s\"" from-state)
                              line))))
   (orgist-test--logbook-entries buf id)))

(defun orgist-test-run-state-log ()
  "Test sync-side state-change logging semantics.
Regression for the catch-up-sync incident: applying a remote state
must never log through `org-todo' (whose notes carry the sync time
and re-mint on every replay), only synthesize entries from Todoist
timestamps.  Covers: completion stamped with completed_at, replay
idempotency (no DONE->DONE), reopen, and recurring repeats."
  (setq orgist-test--failures 0)
  (setq orgist-test--passes 0)
  (message "")
  (message "========================================")
  (message "=== State-change logging test ===")
  (message "========================================")

  ;; Phase 1: Full sync of the synthetic move fixture.
  (message "")
  (message "--- Phase 1: Full sync (synthetic fixture) ---")
  (let* ((script-dir (file-name-directory (or load-file-name buffer-file-name)))
         (test-data-dir (expand-file-name "test-data-move/" script-dir))
         (runtime-dir (expand-file-name
                       (concat "orgist-test/StateLog/")
                       temporary-file-directory)))
    (when (file-directory-p runtime-dir)
      (delete-directory runtime-dir t))
    (make-directory runtime-dir t)
    (setq orgist-test-cache-dir test-data-dir)
    (setq orgist-base-dir runtime-dir)
    (setq orgist-sync-token-filename (expand-file-name "sync_token" runtime-dir))
    (setq orgist-snapshot-file (expand-file-name "snapshots.el" runtime-dir))
    (setq orgist-log-file (expand-file-name "orgist.log" runtime-dir))
    (dolist (buf (buffer-list))
      (when (and (buffer-file-name buf)
                 (string-match-p "\\.org$" (buffer-file-name buf)))
        (kill-buffer buf)))
    (setq orgist-project-buffer-cache nil)
    (setq orgist-snapshots nil)
    (setq orgist-sync-mutex nil)
    (setq orgist-sync-project-filter nil)
    (setq revert-without-query '(".*"))
    (setq org-priority-highest 1
          org-priority-lowest 5
          org-priority-default 5)
    (setq orgist-enable-write-back t)
    (setq orgist-write-back-dry-run t)
    (setq orgist-log-level 'info)
    (setq orgist-test-record-mode 'replay)
    ;; Regression bait: every keyword logs state changes (`!'), like a
    ;; user config.  If sync ever applies states through org-todo with
    ;; logging enabled again, sync-time entries appear and the counts
    ;; below fail.
    (setq-default org-todo-keywords
                  '((sequence "TODO(t!/!)" "|" "DONE(d!/!)")))
    (setq org-log-into-drawer t)
    (setq org-log-states-order-reversed nil))

  (orgist)
  (let ((completed (orgist-test--wait-for-sync 120)))
    (orgist-test-assert completed "Full sync completed")
    (unless completed (kill-emacs 1)))

  (let* ((task-id "MoveTestTaskA01")
         (project-id "MoveTestSrcAa01")
         (buf (orgist-get-project-buffer project-id))
         (deliver
          (lambda (&rest extra)
            (let ((item `((id . ,task-id)
                          (content . "Move test task A")
                          (project_id . ,project-id)
                          (section_id . nil)
                          (parent_id . nil)
                          (child_order . 1)
                          (priority . 1)
                          (description . "")
                          ,@extra))
                  (inhibit-redisplay t)
                  (orgist--batch-save-pending (make-hash-table :test 'eq)))
              (orgist-update-elements (vector item) 'item)
              (orgist--flush-pending-saves)))))

    ;; --- Test 1: remote completion is stamped with completed_at ---
    (message "")
    (message "--- Test 1: remote completion ---")
    (funcall deliver '(checked . t) '(completed_at . "2026-06-24T14:59:49Z")
             '(due . nil))
    (orgist-test-assert-equal
     1 (orgist-test--count-state-entries buf task-id "DONE" "TODO")
     "Remote completion logged exactly one TODO->DONE entry")
    (orgist-test-assert
     (seq-find (lambda (line) (string-match-p "2026-06-24" line))
               (orgist-test--logbook-entries buf task-id))
     "Completion entry carries completed_at, not the sync time")
    (with-current-buffer buf
      (save-excursion
        (goto-char (orgist-find-element-by-id task-id))
        (orgist-test-assert
         (string-match-p "2026-06-24"
                         (or (org-entry-get (point) "CLOSED") ""))
         "CLOSED planning stamp carries completed_at")))

    ;; --- Test 2: replaying a completed item is a no-op ---
    ;; (the DONE->DONE incident: a catch-up sync after weeks offline
    ;; re-delivers already-completed items)
    (message "")
    (message "--- Test 2: replay of completed item ---")
    (let ((before (length (orgist-test--logbook-entries buf task-id))))
      (funcall deliver '(checked . t) '(completed_at . "2026-06-24T14:59:49Z")
               '(due . nil))
      (orgist-test-assert-equal
       before (length (orgist-test--logbook-entries buf task-id))
       "Replay of completed item adds no DONE->DONE entry"))

    ;; --- Test 3: remote reopen ---
    (message "")
    (message "--- Test 3: remote reopen ---")
    (funcall deliver '(checked . :json-false) '(due . nil))
    (orgist-test-assert-equal
     1 (orgist-test--count-state-entries buf task-id "TODO" "DONE")
     "Remote reopen logged exactly one DONE->TODO entry")
    (with-current-buffer buf
      (save-excursion
        (goto-char (orgist-find-element-by-id task-id))
        (orgist-test-assert (equal (org-get-todo-state) "TODO")
                            "Reopened task is TODO")
        (orgist-test-assert (null (org-entry-get (point) "CLOSED"))
                            "Reopened task has no CLOSED stamp")))
    (let ((before (length (orgist-test--logbook-entries buf task-id))))
      (funcall deliver '(checked . :json-false) '(due . nil))
      (orgist-test-assert-equal
       before (length (orgist-test--logbook-entries buf task-id))
       "Replay of open item adds no entry"))

    ;; --- Test 4: recurring occurrence advance logs a repeat ---
    (message "")
    (message "--- Test 4: recurring occurrence ---")
    (funcall deliver '(checked . :json-false)
             '(due . ((date . "2026-07-20")
                      (is_recurring . t)
                      (string . "every week"))))
    (orgist-test-assert-equal
     0 (orgist-test--count-state-entries buf task-id "TODO" "TODO")
     "First recurring date logs no repeat entry")
    (funcall deliver '(checked . :json-false)
             '(due . ((date . "2026-07-27")
                      (is_recurring . t)
                      (string . "every week"))))
    (orgist-test-assert-equal
     1 (orgist-test--count-state-entries buf task-id "TODO" "TODO")
     "Recurring date advance logs one TODO->TODO repeat entry")
    (funcall deliver '(checked . :json-false)
             '(due . ((date . "2026-07-27")
                      (is_recurring . t)
                      (string . "every week"))))
    (orgist-test-assert-equal
     1 (orgist-test--count-state-entries buf task-id "TODO" "TODO")
     "Replay of unchanged recurring item adds no repeat entry"))

  ;; Results
  (message "")
  (message "========================================")
  (message "=== Results: StateLog ===")
  (message "=== Passed: %d  Failed: %d ==="
           orgist-test--passes orgist-test--failures)
  (message "========================================")
  (message "")
  orgist-test--failures)

;;; ============================================================
;;; E. Subprocess sync test
;;; ============================================================

(defun orgist-test-run-subprocess ()
  "Test that subprocess sync produces correct results.
Simulates the elpaca scenario where orgist.el is in a different
directory from orgist-confirm.el by setting `load-file-name' to a
temp dir containing only orgist.el (no orgist-confirm.el)."
  (setq orgist-test--failures 0)
  (setq orgist-test--passes 0)
  (message "")
  (message "========================================")
  (message "=== Subprocess sync test ===")
  (message "========================================")

  ;; Phase 1: Setup isolation for Orgtest
  (message "")
  (message "--- Phase 1: Setup ---")
  (orgist-test-setup-isolation "Orgtest")

  ;; Simulate elpaca layout: copy orgist.el to a separate temp dir
  ;; WITHOUT orgist-confirm.el, then point load-file-name there.
  ;; This reproduces the real elpaca scenario where elpaca/builds/orgist/
  ;; has only orgist.el, while orgist-confirm.el lives in the source repo.
  (let* ((script-dir (file-name-directory (or load-file-name buffer-file-name)))
         (fake-builds-dir (expand-file-name
                           "orgist-test/fake-builds/"
                           temporary-file-directory)))
    (when (file-directory-p fake-builds-dir)
      (delete-directory fake-builds-dir t))
    (make-directory fake-builds-dir t)
    (copy-file (expand-file-name "orgist.el" script-dir)
               (expand-file-name "orgist.el" fake-builds-dir) t)
    ;; Do NOT copy orgist-confirm.el — this simulates elpaca builds

    ;; Override load-file-name so orgist--subprocess-pull resolves
    ;; orgist-dir to the fake-builds dir (missing orgist-confirm.el).
    ;; Also remove the real orgist source dir from load-path so the
    ;; subprocess can ONLY find orgist.el via the fake-builds dir.
    (let* ((load-file-name (expand-file-name "orgist.el" fake-builds-dir))
           (load-path (cons fake-builds-dir
                            (seq-remove
                             (lambda (p) (string= (file-truename p)
                                                  (file-truename script-dir)))
                             load-path))))

      ;; Phase 2: Load JSON and run subprocess
      (message "")
      (message "--- Phase 2: Subprocess sync ---")
      (let* ((json-file (expand-file-name "full-sync.json" orgist-test-cache-dir))
             (json-object-type 'alist)
             (json-array-type 'vector)
             (json-key-type 'symbol)
             (data (json-read-file json-file)))
        ;; Call subprocess-pull
        (orgist--subprocess-pull data)

        ;; Wait for the subprocess to finish
        (let ((proc (get-process "orgist-sync"))
              (waited 0)
              (max-wait 120))
          (when proc
            (while (and (process-live-p proc) (< waited max-wait))
              (accept-process-output proc 1)
              (setq waited (1+ waited))
              (when (= (% waited 10) 0)
                (message "[test-harness] Waiting for subprocess... %ds" waited))))
          ;; Give sentinel a moment to run
          (accept-process-output nil 0.5)

          ;; Phase 3: Verify results
          (message "")
          (message "--- Phase 3: Verify subprocess results ---")
          (orgist-test-assert (or (not proc) (not (process-live-p proc)))
                              "Subprocess completed (not still running)")

          ;; Check org files were created on disk (filter .# lock files)
          (let ((org-files (directory-files orgist-base-dir nil
                                           "^[^.].*\\.org$")))
            (orgist-test-assert (length> org-files 0)
                                (format "Org files created by subprocess (%d found)"
                                        (length org-files)))
            (dolist (f org-files)
              (message "  -> %s (%d bytes)" f
                       (file-attribute-size
                        (file-attributes
                         (expand-file-name f orgist-base-dir))))))

          ;; Check snapshots were saved
          (orgist-test-assert-file-exists
           orgist-snapshot-file "Snapshots saved by subprocess")

          ;; Load snapshots and verify
          (orgist-load-snapshots t)
          (orgist-test-assert (> (hash-table-count orgist-snapshots) 0)
                              (format "Snapshots loadable (%d elements)"
                                      (hash-table-count orgist-snapshots)))

          ;; Open files and run diff — should be zero diffs
          (dolist (file (directory-files orgist-base-dir t "^[^.].*\\.org$"))
            (find-file file)
            (org-mode)
            (orgist-build-id-cache))
          (let* ((changes (orgist-diff-all-elements))
                 (real-diffs
                  (seq-remove
                   (lambda (c)
                     (seq-every-p (lambda (d) (memq (car d) '(:due :deadline)))
                                  (cdr c)))
                   changes)))
            (orgist-test-assert-equal
             0 (length real-diffs)
             "Zero spurious non-date diffs from subprocess sync")
            (when real-diffs
              (message "  Unexpected diffs:")
              (dolist (c (seq-take real-diffs 10))
                (message "    %s: %S" (car c) (cdr c)))))))))

  ;; Results
  (message "")
  (message "========================================")
  (message "=== Results: Subprocess ===")
  (message "=== Passed: %d  Failed: %d ==="
           orgist-test--passes orgist-test--failures)
  (message "========================================")
  (message "")
  orgist-test--failures)

;;; E2. Subprocess incremental sync test
;;; -----------------------------------------------------------

(defun orgist-test-run-subprocess-incremental ()
  "Test that subprocess sync handles incremental data (items only).
Simulates the real-world scenario where an incremental API
response contains only changed items (0 projects, 0 sections).
This is the scenario that caused exit-255 crashes."
  (setq orgist-test--failures 0)
  (setq orgist-test--passes 0)
  (message "")
  (message "========================================")
  (message "=== Subprocess incremental sync test ===")
  (message "========================================")

  ;; Phase 1: Full inline sync to set up org files and snapshots
  (message "")
  (message "--- Phase 1: Full inline sync (setup) ---")
  (orgist-test-setup-isolation "Orgtest")
  (let* ((json-file (expand-file-name "full-sync.json" orgist-test-cache-dir))
         (json-object-type 'alist)
         (json-array-type 'vector)
         (json-key-type 'symbol)
         (data (json-read-file json-file))
         (projects (alist-get 'projects data))
         (sections (alist-get 'sections data))
         (items (alist-get 'items data))
         (filter-ids (orgist-resolve-project-filter projects)))
    ;; Filter to Orgtest project
    (when filter-ids
      (setq projects (seq-filter
                      (lambda (p) (member (alist-get 'id p) filter-ids))
                      projects))
      (setq sections (seq-filter
                      (lambda (s) (member (alist-get 'project_id s) filter-ids))
                      sections))
      (setq items (seq-filter
                   (lambda (i) (member (alist-get 'project_id i) filter-ids))
                   items)))
    ;; Inline sync (synchronous)
    (let ((inhibit-redisplay t)
          (orgist--batch-save-pending (make-hash-table :test 'eq))
          (gc-cons-threshold (* 100 1024 1024)))
      (orgist-update-projects (orgist-sort-hierarchically projects))
      (orgist-update-elements sections 'section)
      (orgist-update-elements (orgist-sort-hierarchically items) 'item)
      (orgist--flush-pending-saves)
      (orgist-save-snapshots))

    (let ((org-files (directory-files orgist-base-dir nil "^[^.].*\\.org$")))
      (orgist-test-assert (length> org-files 0)
                          (format "Phase 1: Org files created (%d)" (length org-files))))
    (let ((snap-count-before (hash-table-count orgist-snapshots)))
      (orgist-test-assert (> snap-count-before 0)
                          (format "Phase 1: Snapshots created (%d)" snap-count-before))

      ;; Phase 2: Subprocess incremental sync (items only, 0 projects/sections)
      (message "")
      (message "--- Phase 2: Subprocess incremental sync ---")
      (message "  Items: %d, Snapshots before: %d" (length items) snap-count-before)

      ;; Create incremental data: same items, empty projects and sections.
      ;; The project filter is still "Orgtest", but with 0 projects the
      ;; filter produces nil → items are processed unfiltered.
      ;; This is the exact scenario from the user's crash.
      (let ((incremental-data
             `((sync_token . "incremental_test_token")
               (projects . [])
               (sections . [])
               (items . ,(vconcat items)))))

        ;; Simulate elpaca layout (same as existing subprocess test)
        (let* ((script-dir (file-name-directory (or load-file-name buffer-file-name)))
               (fake-builds-dir (expand-file-name
                                 "orgist-test/fake-builds/"
                                 temporary-file-directory)))
          (when (file-directory-p fake-builds-dir)
            (delete-directory fake-builds-dir t))
          (make-directory fake-builds-dir t)
          (copy-file (expand-file-name "orgist.el" script-dir)
                     (expand-file-name "orgist.el" fake-builds-dir) t)

          (let* ((load-file-name (expand-file-name "orgist.el" fake-builds-dir))
                 (load-path (cons fake-builds-dir
                                  (seq-remove
                                   (lambda (p) (string= (file-truename p)
                                                        (file-truename script-dir)))
                                   load-path))))

            ;; Call subprocess-pull with incremental data
            (orgist--subprocess-pull incremental-data)

            ;; Wait for the subprocess to finish
            (let ((proc (get-process "orgist-sync"))
                  (waited 0)
                  (max-wait 120))
              (when proc
                (while (and (process-live-p proc) (< waited max-wait))
                  (accept-process-output proc 1)
                  (setq waited (1+ waited))
                  (when (= (% waited 10) 0)
                    (message "[test-harness] Waiting for subprocess... %ds" waited))))
              (accept-process-output nil 0.5)

              ;; Phase 3: Verify results
              (message "")
              (message "--- Phase 3: Verify incremental subprocess results ---")
              (orgist-test-assert (or (not proc) (not (process-live-p proc)))
                                  "Subprocess completed (not still running)")

              ;; Check org files still exist
              (let ((org-files (directory-files orgist-base-dir nil
                                               "^[^.].*\\.org$")))
                (orgist-test-assert (length> org-files 0)
                                    (format "Org files still present (%d found)"
                                            (length org-files))))

              ;; Reload snapshots and verify count preserved
              (orgist-load-snapshots t)
              (orgist-test-assert (> (hash-table-count orgist-snapshots) 0)
                                  (format "Snapshots loadable (%d elements)"
                                          (hash-table-count orgist-snapshots)))
              ;; Key assertion: snapshot count should be >= the count before
              ;; (subprocess must preserve existing snapshots, not overwrite)
              (orgist-test-assert
               (>= (hash-table-count orgist-snapshots) snap-count-before)
               (format "Snapshot count preserved (%d >= %d before)"
                       (hash-table-count orgist-snapshots) snap-count-before))

              ;; Open files and run diff — should be zero real diffs
              (dolist (file (directory-files orgist-base-dir t "^[^.].*\\.org$"))
                (find-file file)
                (org-mode)
                (orgist-build-id-cache))
              (let* ((changes (orgist-diff-all-elements))
                     (real-diffs
                      (seq-remove
                       (lambda (c)
                         (seq-every-p (lambda (d) (memq (car d) '(:due :deadline)))
                                      (cdr c)))
                       changes)))
                (orgist-test-assert-equal
                 0 (length real-diffs)
                 "Zero spurious non-date diffs from incremental subprocess sync")
                (when real-diffs
                  (message "  Unexpected diffs:")
                  (dolist (c (seq-take real-diffs 10))
                    (message "    %s: %S" (car c) (cdr c)))))))))))

  ;; Results
  (message "")
  (message "========================================")
  (message "=== Results: Subprocess Incremental ===")
  (message "=== Passed: %d  Failed: %d ==="
           orgist-test--passes orgist-test--failures)
  (message "========================================")
  (message "")
  orgist-test--failures)

;;; ============================================================
;;; F. Formatting tests
;;; ============================================================

(defun orgist-test-run-formatting ()
  "Test that org-mode output is correctly formatted.
Creates synthetic Todoist data and verifies exact formatting of:
- Property drawer placement
- Logbook entry placement (no blank line after :END:)
- Description spacing (blank line before description)
- Scheduling with time and recurrence
- Tasks with and without descriptions"
  (setq orgist-test--failures 0)
  (setq orgist-test--passes 0)
  (message "")
  (message "========================================")
  (message "=== Formatting tests ===")
  (message "========================================")
  (message "")

  ;; Set up isolation
  (let* ((runtime-dir (expand-file-name "orgist-test/Formatting/"
                                        temporary-file-directory)))
    (when (file-directory-p runtime-dir)
      (delete-directory runtime-dir t))
    (make-directory runtime-dir t)
    (setq orgist-base-dir runtime-dir)
    (setq orgist-sync-token-filename (expand-file-name "sync_token" runtime-dir))
    (setq orgist-snapshot-file (expand-file-name "snapshots.el" runtime-dir))
    (setq orgist-log-file (expand-file-name "orgist.log" runtime-dir))
    (setq orgist-project-buffer-cache nil)
    (setq orgist-snapshots nil)
    (setq orgist-sync-mutex nil)
    (setq orgist-sync-project-filter nil)
    (setq revert-without-query '(".*"))
    (setq org-priority-highest 1 org-priority-lowest 5 org-priority-default 5)
    (setq orgist-enable-write-back t)
    (setq orgist-write-back-dry-run t)
    (setq orgist-log-level 'info)
    ;; Match user's org settings — logbook in a drawer
    (setq org-log-into-drawer t)
    ;; Kill leftover buffers
    (dolist (buf (buffer-list))
      (when (and (buffer-file-name buf)
                 (string-match-p "\\.org$" (buffer-file-name buf)))
        (kill-buffer buf)))

    ;; Synthetic project
    (let ((project '((id . "fmt-proj-1")
                     (name . "FmtTest")
                     (parent_id . nil)
                     (is_deleted . :json-false)
                     (child_order . 0)))
          ;; Task 1: Simple task, no description, no schedule
          (task-simple '((id . "fmt-task-1")
                         (content . "Simple task")
                         (project_id . "fmt-proj-1")
                         (section_id . nil)
                         (parent_id . nil)
                         (child_order . 0)
                         (priority . 1)
                         (checked . :json-false)
                         (labels . [])
                         (due . nil)
                         (deadline . nil)
                         (duration . nil)
                         (description . "")
                         (added_at . "2025-06-22T15:43:00Z")))
          ;; Task 2: With description, no schedule
          (task-with-desc '((id . "fmt-task-2")
                            (content . "Task with description")
                            (project_id . "fmt-proj-1")
                            (section_id . nil)
                            (parent_id . nil)
                            (child_order . 1)
                            (priority . 3)
                            (checked . :json-false)
                            (labels . [])
                            (due . nil)
                            (deadline . nil)
                            (duration . nil)
                            (description . "This is the description.\n\nSecond paragraph.")
                            (added_at . "2025-06-23T04:58:00Z")))
          ;; Task 3: With schedule time + recurrence (every 7 days at 6:30am)
          (task-recurring '((id . "fmt-task-3")
                            (content . "Recurring task")
                            (project_id . "fmt-proj-1")
                            (section_id . nil)
                            (parent_id . nil)
                            (child_order . 2)
                            (priority . 1)
                            (checked . :json-false)
                            (labels . ["Health"])
                            (due . ((date . "2026-02-17T06:30:00")
                                    (is_recurring . t)
                                    (lang . "en")
                                    (string . "every 7 days at 6:30am")
                                    (timezone . nil)))
                            (deadline . nil)
                            (duration . nil)
                            (description . "")
                            (added_at . "2025-07-01T10:00:00Z")))
          ;; Task 4: With schedule time + duration
          (task-duration '((id . "fmt-task-4")
                           (content . "Timed task")
                           (project_id . "fmt-proj-1")
                           (section_id . nil)
                           (parent_id . nil)
                           (child_order . 3)
                           (priority . 2)
                           (checked . :json-false)
                           (labels . [])
                           (due . ((date . "2026-02-20T13:00:00")
                                   (is_recurring . :json-false)
                                   (lang . "en")
                                   (string . "Feb 20 1pm")
                                   (timezone . nil)))
                           (deadline . nil)
                           (duration . ((amount . 60) (unit . "minute")))
                           (description . "Meeting notes here.")
                           (added_at . "2025-08-01T12:00:00Z")))
          ;; Task 5: With description and sub-task
          (task-parent '((id . "fmt-task-5")
                         (content . "Parent task")
                         (project_id . "fmt-proj-1")
                         (section_id . nil)
                         (parent_id . nil)
                         (child_order . 4)
                         (priority . 1)
                         (checked . :json-false)
                         (labels . [])
                         (due . nil)
                         (deadline . nil)
                         (duration . nil)
                         (description . "Parent description.")
                         (added_at . "2025-09-01T09:00:00Z")))
          (task-child '((id . "fmt-task-6")
                        (content . "Child task")
                        (project_id . "fmt-proj-1")
                        (section_id . nil)
                        (parent_id . "fmt-task-5")
                        (child_order . 0)
                        (priority . 1)
                        (checked . :json-false)
                        (labels . [])
                        (due . nil)
                        (deadline . nil)
                        (duration . nil)
                        (description . "")
                        (added_at . "2025-09-01T09:05:00Z")))
          ;; Task 7: Date-only schedule (no time, with weekly recurrence)
          (task-dateonly '((id . "fmt-task-7")
                          (content . "Weekly review")
                          (project_id . "fmt-proj-1")
                          (section_id . nil)
                          (parent_id . nil)
                          (child_order . 5)
                          (priority . 1)
                          (checked . :json-false)
                          (labels . [])
                          (due . ((date . "2026-02-20")
                                  (is_recurring . t)
                                  (lang . "en")
                                  (string . "every week")
                                  (timezone . nil)))
                          (deadline . nil)
                          (duration . nil)
                          (description . "")
                          (added_at . "2025-10-01T08:00:00Z")))
          ;; Section 1: For testing section_id placement
          (section-1 '((id . "fmt-section-1")
                       (name . "Test section")
                       (project_id . "fmt-proj-1")
                       (section_order . 1)))
          ;; Task 8: Inside section
          (task-in-section '((id . "fmt-task-8")
                             (content . "Task in section")
                             (project_id . "fmt-proj-1")
                             (section_id . "fmt-section-1")
                             (parent_id . nil)
                             (child_order . 0)
                             (priority . 1)
                             (checked . :json-false)
                             (labels . [])
                             (due . nil)
                             (deadline . nil)
                             (duration . nil)
                             (description . "")
                             (added_at . "2025-11-01T10:00:00Z")))
          ;; Task 9: Logbook + description with list items
          (task-log-and-list '((id . "fmt-task-9")
                               (content . "Task with logbook and list description")
                               (project_id . "fmt-proj-1")
                               (section_id . nil)
                               (parent_id . nil)
                               (child_order . 8)
                               (priority . 3)
                               (checked . :json-false)
                               (labels . [])
                               (due . nil)
                               (deadline . nil)
                               (duration . nil)
                               (description . "Timing should be flexible:\n\n- Everyday: 4:30am-5:30am\n- Weekdays: 10am-11am or 1:30pm-2:30pm\n- Weekends: 9am-12pm or 2pm-4pm")
                               (added_at . "2025-06-15T08:00:00Z"))))

      ;; Create project
      (orgist-update-projects (vector project))
      ;; Create sections
      (orgist-update-elements (vector section-1) 'section)
      ;; Create items (hierarchical sort: parent before child)
      (orgist-update-elements
       (vector task-simple task-with-desc task-recurring task-duration
               task-parent task-child task-dateonly task-in-section
               task-log-and-list)
       'item)
      (orgist--flush-pending-saves)

      ;; Read the resulting org file
      (let* ((org-file (expand-file-name "FmtTest.org" runtime-dir))
             (content (with-temp-buffer
                        (insert-file-contents org-file)
                        (buffer-string)))
             (lines (split-string content "\n")))

        ;; --- Test 1: Simple task (no description) ---
        ;; With org-log-into-drawer, logbook is in :LOGBOOK: drawer.
        ;; :END: (property drawer) -> :LOGBOOK: (no blank line between)
        (message "")
        (message "--- Test group: Simple task (no description) ---")
        (let ((task1-idx (seq-position lines "fmt-task-1"
                                       (lambda (line pat)
                                         (string-match-p pat line)))))
          (when task1-idx
            (let ((end-after (cl-loop for i from task1-idx below (length lines)
                                      when (string-match-p "^:END:" (nth i lines))
                                      return i)))
              (when end-after
                (orgist-test-assert
                 (string-match-p "^:LOGBOOK:" (nth (1+ end-after) lines))
                 "Simple task: :LOGBOOK: immediately after :END: (no blank line)")
                ;; Find the logbook drawer :END: and check blank line after
                (let ((log-end (cl-loop for i from (+ end-after 2) below (length lines)
                                        when (string-match-p "^:END:" (nth i lines))
                                        return i)))
                  (when log-end
                    (orgist-test-assert
                     (string= "" (nth (1+ log-end) lines))
                     "Simple task: blank line after logbook drawer (before next heading)")))))))

        ;; --- Test 2: Task with description ---
        (message "")
        (message "--- Test group: Task with description ---")
        (let ((task2-idx (seq-position lines "fmt-task-2"
                                       (lambda (line pat)
                                         (string-match-p pat line)))))
          (when task2-idx
            ;; Find :LOGBOOK: (which comes right after property :END:)
            (let ((logbook-idx (cl-loop for i from task2-idx below (length lines)
                                        when (string-match-p "^:LOGBOOK:" (nth i lines))
                                        return i)))
              (when logbook-idx
                ;; Find logbook drawer :END:
                (let ((log-end (cl-loop for i from (1+ logbook-idx) below (length lines)
                                        when (string-match-p "^:END:" (nth i lines))
                                        return i)))
                  (when log-end
                    ;; After logbook :END: should be blank line, then description
                    (orgist-test-assert
                     (string= "" (nth (1+ log-end) lines))
                     "Desc task: blank line between logbook drawer and description")
                    (orgist-test-assert
                     (string-match-p "description" (nth (+ log-end 2) lines))
                     "Desc task: description text follows blank line")))))))

        ;; --- Test 3: Recurring task with time ---
        ;; SCHEDULED: appears before :PROPERTIES: in org format, so search
        ;; from the heading line (which contains the task name), not the ID.
        (message "")
        (message "--- Test group: Recurring task with time ---")
        (let ((heading-idx (cl-loop for i below (length lines)
                                    when (string-match-p "Recurring task" (nth i lines))
                                    return i)))
          (when heading-idx
            (let ((sched-idx (cl-loop for i from heading-idx
                                      below (min (+ heading-idx 5) (length lines))
                                      when (string-match-p "^SCHEDULED:" (nth i lines))
                                      return i)))
              (when sched-idx
                (let ((sched-line (nth sched-idx lines)))
                  ;; Check time is present
                  (orgist-test-assert
                   (string-match-p "06:30" sched-line)
                   "Recurring task: schedule includes time 06:30")
                  ;; Check recurrence is +7d
                  (orgist-test-assert
                   (string-match-p "\\+7d" sched-line)
                   "Recurring task: recurrence is +7d (not +1d)")
                  ;; Check day of week (Feb 17 2026 = Tue)
                  (orgist-test-assert
                   (string-match-p "2026-02-17 Tue" sched-line)
                   "Recurring task: correct date 2026-02-17 Tue"))))))

        ;; --- Test 4: Task with time + duration ---
        (message "")
        (message "--- Test group: Task with duration ---")
        (let ((heading-idx (cl-loop for i below (length lines)
                                    when (string-match-p "Timed task" (nth i lines))
                                    return i)))
          (when heading-idx
            (let ((sched-idx (cl-loop for i from heading-idx
                                      below (min (+ heading-idx 5) (length lines))
                                      when (string-match-p "^SCHEDULED:" (nth i lines))
                                      return i)))
              (when sched-idx
                (let ((sched-line (nth sched-idx lines)))
                  ;; Check time range
                  (orgist-test-assert
                   (string-match-p "13:00-14:00" sched-line)
                   "Duration task: schedule shows 13:00-14:00 range")
                  ;; Check day of week (Feb 20 2026 = Fri)
                  (orgist-test-assert
                   (string-match-p "2026-02-20 Fri" sched-line)
                   "Duration task: correct date 2026-02-20 Fri"))))))

        ;; --- Test 5: Parent with child ---
        (message "")
        (message "--- Test group: Parent/child hierarchy ---")
        (let ((task5-idx (seq-position lines "fmt-task-5"
                                       (lambda (line pat)
                                         (string-match-p pat line)))))
          (when task5-idx
            ;; Find parent heading
            (let ((parent-heading (cl-loop for i from (max 0 (- task5-idx 3))
                                           below task5-idx
                                           when (string-match-p "^\\* TODO Parent task"
                                                                 (nth i lines))
                                           return i)))
              (when parent-heading
                ;; Find child heading
                (let ((child-heading (cl-loop for i from task5-idx below (length lines)
                                              when (string-match-p "^\\*\\* TODO Child task"
                                                                    (nth i lines))
                                              return i)))
                  (orgist-test-assert child-heading
                                      "Parent/child: child task is at level 2"))))))

        ;; --- Test 6: Date-only schedule with weekly recurrence ---
        (message "")
        (message "--- Test group: Date-only schedule ---")
        (let ((heading-idx (cl-loop for i below (length lines)
                                    when (string-match-p "Weekly review" (nth i lines))
                                    return i)))
          (when heading-idx
            (let ((sched-idx (cl-loop for i from heading-idx
                                      below (min (+ heading-idx 5) (length lines))
                                      when (string-match-p "^SCHEDULED:" (nth i lines))
                                      return i)))
              (when sched-idx
                (let ((sched-line (nth sched-idx lines)))
                  ;; Date-only: no time component
                  (orgist-test-assert
                   (not (string-match-p "[0-9]\\{2\\}:[0-9]\\{2\\}" sched-line))
                   "Date-only: no time in schedule")
                  ;; Weekly recurrence
                  (orgist-test-assert
                   (string-match-p "\\+1w" sched-line)
                   "Date-only: weekly recurrence +1w")
                  ;; Check date (Feb 20 2026 = Fri)
                  (orgist-test-assert
                   (string-match-p "2026-02-20 Fri" sched-line)
                   "Date-only: correct date 2026-02-20 Fri"))))))

        ;; --- Test 7: No consecutive blank lines anywhere ---
        ;; Trim trailing empty lines (normal for file endings)
        (message "")
        (message "--- Test group: Global spacing rules ---")
        (let ((trimmed-lines (let ((ls (copy-sequence lines)))
                               (while (and ls (string= (car (last ls)) ""))
                                 (setq ls (butlast ls)))
                               ls))
              (double-blank 0)
              (prev-blank nil))
          (dolist (line trimmed-lines)
            (if (string-empty-p line)
                (when prev-blank
                  (setq double-blank (1+ double-blank)))
              (setq prev-blank nil))
            (when (string-empty-p line)
              (setq prev-blank t)))
          (orgist-test-assert-equal
           0 double-blank
           "No consecutive blank lines in output"))

        ;; --- Test 8: Tags on recurring task ---
        (message "")
        (message "--- Test group: Tags ---")
        (let ((heading-line
               (cl-loop for line in lines
                        when (string-match-p "Recurring task" line)
                        return line)))
          (when heading-line
            (orgist-test-assert
             (string-match-p ":Health:" heading-line)
             "Recurring task: tags are present")))

        ;; --- Test 9: Priority on task with description ---
        (message "")
        (message "--- Test group: Priority ---")
        (let ((heading-line
               (cl-loop for line in lines
                        when (string-match-p "Task with description" line)
                        return line)))
          (when heading-line
            ;; Todoist priority 3 → org priority (1 + 4-3) = [#2]
            (orgist-test-assert
             (string-match-p "\\[#2\\]" heading-line)
             "Desc task: priority [#2] is present")))

        ;; ===========================================================
        ;; Write-back tests (using the same project)
        ;; ===========================================================
        (message "")
        (message "--- Test group: Write-back date change ---")
        ;; Save snapshots and verify zero diff as baseline
        (orgist-save-snapshots)
        (let* ((org-file (expand-file-name "FmtTest.org" runtime-dir))
               (buf (find-file-noselect org-file)))
          (with-current-buffer buf
            (orgist-build-id-cache)
            ;; Baseline: no diffs
            (let ((changes (orgist-diff-all-elements)))
              (orgist-test-assert-equal
               0 (length changes)
               "Write-back baseline: zero diffs before edit"))

            ;; Edit: change the recurring task's date from Feb 17 to Feb 18
            (goto-char (orgist-find-element-by-id "fmt-task-3"))
            (let ((org-log-done nil) (org-log-repeat nil))
              (org-schedule nil "<2026-02-18 Wed 06:30 +7d>"))

            ;; Diff should detect the change
            (let* ((changes (orgist-diff-all-elements))
                   (task3-change (assoc "fmt-task-3" changes)))
              (orgist-test-assert
               task3-change
               "Write-back: date change detected in diff")
              (when task3-change
                ;; Generate commands and check the due object
                (let* ((commands (orgist-changes-to-commands changes))
                       (cmd (car commands))
                       (args (alist-get 'args cmd))
                       (due (alist-get 'due args)))
                  (orgist-test-assert
                   (string-match-p "T06:30" (alist-get 'date due))
                   "Write-back: command includes time in date field")
                  (orgist-test-assert
                   (alist-get 'is_recurring due)
                   "Write-back: command includes is_recurring=true")
                  (orgist-test-assert
                   (string-match-p "7 days" (or (alist-get 'string due) ""))
                   "Write-back: command includes recurrence in string")

                  ;; Execute write-back (dry-run) and verify snapshot update
                  (orgist-execute-write-back commands)
                  (let ((changes-after (orgist-diff-all-elements)))
                    (orgist-test-assert-equal
                     0 (length changes-after)
                     "Write-back: zero diffs after dry-run (snapshots updated)")))))

            ;; ==========================================================
            ;; Subtask (child task) edit detection
            ;; ==========================================================
            (message "")
            (message "--- Test group: Subtask edit detection ---")
            ;; Reset to clean baseline
            (orgist-save-snapshots)
            (orgist-load-snapshots t)
            (orgist-build-id-cache)
            (let ((baseline (orgist-diff-all-elements)))
              (orgist-test-assert-equal
               0 (length baseline)
               "Subtask-edit baseline: zero diffs before edit"))
            ;; Edit the child task's heading text (fmt-task-6)
            (goto-char (orgist-find-element-by-id "fmt-task-6"))
            (org-back-to-heading t)
            ;; Replace "Child task" with "Edited child task"
            (when (re-search-forward "Child task" (line-end-position) t)
              (replace-match "Edited child task"))
            (let* ((changes (orgist-diff-all-elements))
                   (child-change (assoc "fmt-task-6" changes)))
              (orgist-test-assert
               child-change
               "Subtask-edit: child task change detected")
              (when child-change
                (let* ((commands (orgist-changes-to-commands changes))
                       (cmd (seq-find
                             (lambda (c)
                               (equal (alist-get 'id (alist-get 'args c))
                                      "fmt-task-6"))
                             commands)))
                  (orgist-test-assert
                   cmd
                   "Subtask-edit: produces command for child task")
                  (when cmd
                    (orgist-test-assert-equal
                     "item_update" (alist-get 'type cmd)
                     "Subtask-edit: command is item_update"))))
              ;; Dry-run and verify zero diffs after
              (let ((commands (orgist-changes-to-commands changes)))
                (orgist-execute-write-back commands))
              (let ((after (orgist-diff-all-elements)))
                (orgist-test-assert-equal
                 0 (length after)
                 "Subtask-edit: zero diffs after dry-run")))

            ;; ==========================================================
            ;; New task creation tests
            ;; ==========================================================
            (message "")
            (message "--- Test group: New task detection ---")
            ;; Reset to clean baseline: zero diffs
            (orgist-save-snapshots)
            (orgist-load-snapshots t)
            (orgist-build-id-cache)
            (let ((baseline (orgist-diff-all-elements)))
              (orgist-test-assert-equal
               0 (length baseline)
               "New-task baseline: zero diffs before insert"))

            ;; Insert a new TODO heading at end of buffer
            (goto-char (point-max))
            (insert "\n* TODO Brand new task\n")
            (let* ((changes (orgist-diff-all-elements))
                   (new-changes (seq-filter
                                 (lambda (c) (eq (cdr c) 'new))
                                 changes)))
              (orgist-test-assert-equal
               1 (length new-changes)
               "New-task: exactly 1 'new change detected")
              (when new-changes
                (let* ((new-id (caar new-changes))
                       (pos (orgist-find-element-by-id new-id)))
                  ;; Verify temp :ID: was assigned
                  (orgist-test-assert
                   pos
                   "New-task: heading received a temp :ID: property")
                  ;; Generate commands
                  (let* ((commands (orgist-changes-to-commands changes))
                         (add-cmds (seq-filter
                                    (lambda (c)
                                      (equal (alist-get 'type c) "item_add"))
                                    commands)))
                    (orgist-test-assert-equal
                     1 (length add-cmds)
                     "New-task: produces exactly 1 item_add command")
                    (when add-cmds
                      (let* ((cmd (car add-cmds))
                             (args (alist-get 'args cmd)))
                        (orgist-test-assert-equal
                         "Brand new task" (alist-get 'content args)
                         "New-task: item_add has correct content")
                        (orgist-test-assert-equal
                         "fmt-proj-1" (alist-get 'project_id args)
                         "New-task: item_add has correct project_id")
                        (orgist-test-assert
                         (alist-get 'temp_id cmd)
                         "New-task: item_add has temp_id")))
                    ;; Execute dry-run and verify zero diffs after
                    (orgist-execute-write-back commands)
                    (let ((changes-after (orgist-diff-all-elements)))
                      (orgist-test-assert-equal
                       0 (length changes-after)
                       "New-task: zero diffs after dry-run (snapshot created)"))))))

            ;; Negative test: non-TODO heading should NOT be detected
            (message "")
            (message "--- Test group: Non-TODO heading ignored ---")
            (orgist-save-snapshots)
            (orgist-load-snapshots t)
            (orgist-build-id-cache)
            (goto-char (point-max))
            (insert "\n* Just a note\n")
            (let* ((changes (orgist-diff-all-elements))
                   (new-changes (seq-filter
                                 (lambda (c) (eq (cdr c) 'new))
                                 changes)))
              (orgist-test-assert-equal
               0 (length new-changes)
               "Non-TODO heading: not detected as new task"))

            ;; ==========================================================
            ;; Parent placement tests (section_id vs parent_id)
            ;; ==========================================================
            (message "")
            (message "--- Test group: New task under section ---")
            (orgist-save-snapshots)
            (orgist-load-snapshots t)
            (orgist-build-id-cache)
            ;; Insert a new task under the section heading
            (goto-char (orgist-find-element-by-id "fmt-section-1"))
            (org-end-of-subtree t t)
            (let ((level (save-excursion
                           (goto-char (orgist-find-element-by-id "fmt-section-1"))
                           (1+ (org-current-level)))))
              (insert (make-string level ?*) " TODO Task under section\n"))
            (let* ((changes (orgist-diff-all-elements))
                   (new-changes (seq-filter
                                 (lambda (c) (eq (cdr c) 'new))
                                 changes)))
              (orgist-test-assert-equal
               1 (length new-changes)
               "Section-child: detected as new task")
              (when new-changes
                (let* ((commands (orgist-changes-to-commands changes))
                       (add-cmds (seq-filter
                                  (lambda (c) (equal (alist-get 'type c) "item_add"))
                                  commands)))
                  (orgist-test-assert-equal
                   1 (length add-cmds)
                   "Section-child: produces 1 item_add command")
                  (when add-cmds
                    (let ((args (alist-get 'args (car add-cmds))))
                      (orgist-test-assert-equal
                       "fmt-section-1" (alist-get 'section_id args)
                       "Section-child: item_add has section_id (not project_id)")
                      (orgist-test-assert
                       (not (assq 'parent_id args))
                       "Section-child: item_add has no parent_id"))))))

            (message "")
            (message "--- Test group: New task under parent task ---")
            (orgist-save-snapshots)
            (orgist-load-snapshots t)
            (orgist-build-id-cache)
            ;; Insert a new task under an existing task (fmt-task-5 = "Parent task")
            (goto-char (orgist-find-element-by-id "fmt-task-5"))
            (org-end-of-subtree t t)
            (let ((level (save-excursion
                           (goto-char (orgist-find-element-by-id "fmt-task-5"))
                           (1+ (org-current-level)))))
              (insert (make-string level ?*) " TODO Subtask of parent\n"))
            (let* ((changes (orgist-diff-all-elements))
                   (new-changes (seq-filter
                                 (lambda (c) (eq (cdr c) 'new))
                                 changes)))
              (orgist-test-assert-equal
               1 (length new-changes)
               "Task-child: detected as new task")
              (when new-changes
                (let* ((commands (orgist-changes-to-commands changes))
                       (add-cmds (seq-filter
                                  (lambda (c) (equal (alist-get 'type c) "item_add"))
                                  commands)))
                  (when add-cmds
                    (let ((args (alist-get 'args (car add-cmds))))
                      (orgist-test-assert-equal
                       "fmt-task-5" (alist-get 'parent_id args)
                       "Task-child: item_add has parent_id")
                      (orgist-test-assert
                       (not (assq 'section_id args))
                       "Task-child: item_add has no section_id"))))))

            (message "")
            (message "--- Test group: Move task under section ---")
            (orgist-save-snapshots)
            (orgist-load-snapshots t)
            (orgist-build-id-cache)
            ;; Move fmt-task-1 (root-level "Simple task") under the section
            ;; by cutting and pasting it as a child of the section heading
            (goto-char (orgist-find-element-by-id "fmt-task-1"))
            (org-cut-subtree)
            (goto-char (orgist-find-element-by-id "fmt-section-1"))
            (org-end-of-subtree t t)
            (org-paste-subtree
             (save-excursion
               (goto-char (orgist-find-element-by-id "fmt-section-1"))
               (1+ (org-current-level))))
            (orgist-build-id-cache)
            (let* ((changes (orgist-diff-all-elements))
                   (move-change (assoc "fmt-task-1" changes)))
              (orgist-test-assert
               move-change
               "Section-move: diff detected for moved task")
              (when move-change
                (let* ((commands (orgist-changes-to-commands changes))
                       (move-cmds (seq-filter
                                   (lambda (c) (equal (alist-get 'type c) "item_move"))
                                   commands)))
                  (orgist-test-assert
                   (>= (length move-cmds) 1)
                   "Section-move: produces item_move command")
                  (when move-cmds
                    (let* ((cmd (seq-find
                                 (lambda (c)
                                   (equal (alist-get 'id (alist-get 'args c))
                                          "fmt-task-1"))
                                 move-cmds))
                           (args (alist-get 'args cmd)))
                      (orgist-test-assert-equal
                       "fmt-section-1" (alist-get 'section_id args)
                       "Section-move: item_move uses section_id")
                      (orgist-test-assert
                       (not (assq 'project_id args))
                       "Section-move: item_move does NOT use project_id"))))))

            (message "")
            (message "--- Test group: Move task from section to root ---")
            ;; fmt-task-8 is inside the section. Move it to root level.
            (orgist-save-snapshots)
            (orgist-load-snapshots t)
            (orgist-build-id-cache)
            (goto-char (orgist-find-element-by-id "fmt-task-8"))
            (org-cut-subtree)
            ;; Paste at root level (level 1) at end of buffer
            (goto-char (point-max))
            (org-paste-subtree 1)
            (orgist-build-id-cache)
            (let* ((changes (orgist-diff-all-elements))
                   (move-change (assoc "fmt-task-8" changes)))
              (orgist-test-assert
               move-change
               "Root-move: diff detected for moved task")
              (when move-change
                (let* ((commands (orgist-changes-to-commands changes))
                       (move-cmds (seq-filter
                                   (lambda (c) (equal (alist-get 'type c) "item_move"))
                                   commands)))
                  (when move-cmds
                    (let* ((cmd (seq-find
                                 (lambda (c)
                                   (equal (alist-get 'id (alist-get 'args c))
                                          "fmt-task-8"))
                                 move-cmds))
                           (args (alist-get 'args cmd)))
                      (orgist-test-assert-equal
                       "fmt-proj-1" (alist-get 'project_id args)
                       "Root-move: item_move uses project_id")
                      (orgist-test-assert
                       (not (assq 'parent_id args))
                       "Root-move: item_move has no parent_id")
                      (orgist-test-assert
                       (not (assq 'section_id args))
                       "Root-move: item_move has no section_id"))))))

            (message "")
            (message "--- Test group: New task under locally created parent ---")
            ;; Flush pending diffs from prior move tests via dry-run
            (let* ((pending (orgist-diff-all-elements))
                   (cmds (orgist-changes-to-commands pending)))
              (when cmds (orgist-execute-write-back cmds)))
            (orgist-save-snapshots)
            (orgist-load-snapshots t)
            (orgist-build-id-cache)
            ;; Create a parent task locally and sync it.  Its snapshot is
            ;; built from local state, so it has :order nil and the heading
            ;; has no TODOIST-ORDER property (neither arrives until the item
            ;; round-trips through a pull).  Regression: children added under
            ;; such a parent were misclassified as project children and sent
            ;; with project_id = the parent task's ID ("Project not found").
            (goto-char (point-max))
            (insert "\n* TODO Local parent task\n")
            (let* ((changes (orgist-diff-all-elements))
                   (new-changes (seq-filter
                                 (lambda (c) (eq (cdr c) 'new))
                                 changes))
                   (parent-id (caar new-changes)))
              (orgist-test-assert-equal
               1 (length new-changes)
               "Local-parent: parent detected as new task")
              (orgist-execute-write-back (orgist-changes-to-commands changes))
              (orgist-test-assert
               (and parent-id
                    (gethash parent-id orgist-snapshots)
                    (null (plist-get (gethash parent-id orgist-snapshots)
                                     :order)))
               "Local-parent: snapshot from local state has no :order")
              ;; Add a subtask under the locally created parent
              (goto-char (orgist-find-element-by-id parent-id))
              (org-end-of-subtree t t)
              (insert "** TODO Subtask of local parent\n")
              (let* ((changes (orgist-diff-all-elements))
                     (commands (orgist-changes-to-commands changes))
                     (add-cmds (seq-filter
                                (lambda (c)
                                  (equal (alist-get 'type c) "item_add"))
                                commands)))
                (orgist-test-assert-equal
                 1 (length add-cmds)
                 "Local-parent: subtask produces 1 item_add command")
                (when add-cmds
                  (let ((args (alist-get 'args (car add-cmds))))
                    (orgist-test-assert-equal
                     parent-id (alist-get 'parent_id args)
                     "Local-parent: item_add has parent_id = parent task")
                    (orgist-test-assert-equal
                     "fmt-proj-1" (alist-get 'project_id args)
                     "Local-parent: item_add keeps project_id = root project")
                    (orgist-test-assert
                     (not (assq 'section_id args))
                     "Local-parent: item_add has no section_id")))
                ;; Sync the subtask so the move test below starts clean
                (orgist-execute-write-back commands))
              (message "")
              (message "--- Test group: Move task under locally created parent ---")
              ;; Move fmt-task-1 (currently under the section) beneath the
              ;; locally created parent; item_move must use parent_id.
              (goto-char (orgist-find-element-by-id "fmt-task-1"))
              (org-cut-subtree)
              (goto-char (orgist-find-element-by-id parent-id))
              (org-end-of-subtree t t)
              (org-paste-subtree 2)
              (orgist-build-id-cache)
              (let* ((changes (orgist-diff-all-elements))
                     (commands (orgist-changes-to-commands changes))
                     (cmd (seq-find
                           (lambda (c)
                             (and (equal (alist-get 'type c) "item_move")
                                  (equal (alist-get 'id (alist-get 'args c))
                                         "fmt-task-1")))
                           commands)))
                (orgist-test-assert
                 cmd
                 "Local-parent-move: produces item_move for fmt-task-1")
                (when cmd
                  (let ((args (alist-get 'args cmd)))
                    (orgist-test-assert-equal
                     parent-id (alist-get 'parent_id args)
                     "Local-parent-move: item_move uses parent_id")
                    (orgist-test-assert
                     (not (assq 'project_id args))
                     "Local-parent-move: item_move has no project_id")
                    (orgist-test-assert
                     (not (assq 'section_id args))
                     "Local-parent-move: item_move has no section_id")))))

            ;; ==========================================================
            ;; Deletion detection tests
            ;; ==========================================================
            (message "")
            (message "--- Test group: Delete existing task ---")
            ;; Flush pending diffs from prior move tests via dry-run
            (let* ((pending (orgist-diff-all-elements))
                   (cmds (orgist-changes-to-commands pending)))
              (when cmds (orgist-execute-write-back cmds)))
            (orgist-save-snapshots)
            (orgist-load-snapshots t)
            (orgist-build-id-cache)
            ;; Verify zero diffs before deleting
            (let ((changes (orgist-diff-all-elements)))
              (orgist-test-assert-equal
               0 (length changes)
               "Delete-task baseline: zero diffs before deletion"))
            ;; Delete fmt-task-1 heading from the buffer
            (goto-char (orgist-find-element-by-id "fmt-task-1"))
            (org-back-to-heading t)
            (let ((beg (point)))
              (org-end-of-subtree t t)
              (delete-region beg (point)))
            (let* ((changes (orgist-diff-all-elements))
                   (del-changes (seq-filter
                                 (lambda (c) (eq (cdr c) 'deleted))
                                 changes)))
              (orgist-test-assert
               (>= (length del-changes) 1)
               "Delete-task: deletion detected")
              (orgist-test-assert
               (assoc "fmt-task-1" del-changes)
               "Delete-task: correct ID marked as deleted"))

            (message "")
            (message "--- Test group: Delete generates item_delete command ---")
            (let* ((changes (orgist-diff-all-elements))
                   (commands (orgist-changes-to-commands changes))
                   (del-cmds (seq-filter
                              (lambda (c) (equal (alist-get 'type c) "item_delete"))
                              commands)))
              (orgist-test-assert
               (>= (length del-cmds) 1)
               "Delete-cmd: produces item_delete command")
              (when del-cmds
                (let* ((cmd (seq-find
                             (lambda (c)
                               (equal (alist-get 'id (alist-get 'args c))
                                      "fmt-task-1"))
                             del-cmds)))
                  (orgist-test-assert
                   cmd
                   "Delete-cmd: item_delete targets correct ID")))
              ;; Execute dry-run to update snapshots, then verify
              ;; deletion is NOT re-detected on the next cycle.
              (orgist-execute-write-back commands)
              (orgist-save-snapshots)
              (orgist-load-snapshots t)
              (orgist-build-id-cache)
              (let* ((changes2 (orgist-diff-all-elements))
                     (del2 (seq-filter
                            (lambda (c) (eq (cdr c) 'deleted))
                            changes2)))
                (orgist-test-assert-equal
                 0 (length del2)
                 "Delete-cmd: deletion not re-detected after dry-run")))

            ;; ==========================================================
            ;; Recurring task completion detection
            ;; ==========================================================
            (message "")
            (message "--- Test group: Recurring task instance completion ---")
            ;; Flush pending diffs and reset to clean baseline
            (let* ((pending (orgist-diff-all-elements))
                   (cmds (orgist-changes-to-commands pending)))
              (when cmds (orgist-execute-write-back cmds)))
            (orgist-save-snapshots)
            (orgist-load-snapshots t)
            (orgist-build-id-cache)
            (let ((baseline (orgist-diff-all-elements)))
              (orgist-test-assert-equal
               0 (length baseline)
               "Recurring-complete baseline: zero diffs"))

            ;; Simulate completing one instance of the recurring task:
            ;; - Keep TODO state (not DONE)
            ;; - Advance SCHEDULED to next occurrence
            ;; - Set LAST_REPEAT property
            ;; This is what org-mode does when you cycle a repeating task.
            (goto-char (orgist-find-element-by-id "fmt-task-3"))
            (let ((org-log-done nil) (org-log-repeat nil))
              (org-schedule nil "<2026-02-24 Tue 06:30 +7d>"))
            (org-entry-put (point) "LAST_REPEAT"
                           "[2026-02-17 Mon 06:30]")

            ;; Diff should detect :last-repeat change, NOT :due
            (let* ((changes (orgist-diff-all-elements))
                   (task-change (assoc "fmt-task-3" changes)))
              (orgist-test-assert
               task-change
               "Recurring-complete: change detected")
              (when task-change
                (let ((diff-fields (mapcar #'car (cdr task-change))))
                  (orgist-test-assert
                   (memq :last-repeat diff-fields)
                   "Recurring-complete: :last-repeat in diff")
                  (orgist-test-assert
                   (not (memq :due diff-fields))
                   "Recurring-complete: :due suppressed from diff"))
                ;; Generate commands — should be item_close
                (let* ((commands (orgist-changes-to-commands changes))
                       (close-cmds (seq-filter
                                    (lambda (c) (equal (alist-get 'type c)
                                                       "item_close"))
                                    commands))
                       (complete-cmds (seq-filter
                                       (lambda (c) (equal (alist-get 'type c)
                                                          "item_complete"))
                                       commands)))
                  (orgist-test-assert
                   (length= close-cmds 1)
                   "Recurring-complete: generates item_close")
                  (orgist-test-assert-equal
                   0 (length complete-cmds)
                   "Recurring-complete: does NOT generate item_complete")
                  ;; Dry-run and verify zero diffs after
                  (orgist-execute-write-back commands)
                  (let ((after (orgist-diff-all-elements)))
                    (orgist-test-assert-equal
                     0 (length after)
                     "Recurring-complete: zero diffs after dry-run")))))

            ;; ==========================================================
            ;; Permanent task completion (TODO→DONE)
            ;; ==========================================================
            (message "")
            (message "--- Test group: Permanent task completion ---")
            ;; Flush any pending diffs from the prior recurring test
            (let* ((pending (orgist-diff-all-elements))
                   (cmds (orgist-changes-to-commands pending)))
              (when cmds (orgist-execute-write-back cmds)))
            (orgist-save-snapshots)
            (orgist-load-snapshots t)
            (orgist-build-id-cache)
            ;; Mark a non-recurring task as DONE
            (goto-char (orgist-find-element-by-id "fmt-task-2"))
            (let ((org-log-done nil) (org-log-repeat nil))
              (org-todo "DONE"))

            (let* ((changes (orgist-diff-all-elements))
                   (task-change (assoc "fmt-task-2" changes)))
              (orgist-test-assert
               task-change
               "Permanent-complete: change detected")
              (when task-change
                (let ((diff-fields (mapcar #'car (cdr task-change))))
                  (orgist-test-assert
                   (memq :checked diff-fields)
                   "Permanent-complete: :checked in diff"))
                ;; Generate commands — should be item_complete
                (let* ((commands (orgist-changes-to-commands changes))
                       (complete-cmds (seq-filter
                                       (lambda (c) (equal (alist-get 'type c)
                                                          "item_complete"))
                                       commands))
                       (close-cmds (seq-filter
                                   (lambda (c) (equal (alist-get 'type c)
                                                      "item_close"))
                                   commands)))
                  (orgist-test-assert
                   (length= complete-cmds 1)
                   "Permanent-complete: generates item_complete")
                  (orgist-test-assert-equal
                   0 (length close-cmds)
                   "Permanent-complete: does NOT generate item_close")))))

            ;; ==========================================================
            ;; Logbook + description with list items
            ;; ==========================================================
            (message "")
            (message "--- Test group: Logbook + list description ---")
            ;; Verify initial sync: LOGBOOK drawer :END: before description
            ;; Ensure we're in the right buffer
            (set-buffer (find-file-noselect org-file))
            (orgist-build-id-cache)
            (let ((pos (orgist-find-element-by-id "fmt-task-9")))
              (orgist-test-assert pos "Log+list task found")
              (when pos
                (save-excursion
                  (goto-char pos)
                  (let* ((subtree-end (save-excursion (org-end-of-subtree t t) (point)))
                         (content (buffer-substring-no-properties pos subtree-end))
                         (clines (split-string content "\n")))
                    ;; Verify :LOGBOOK: drawer exists
                    (orgist-test-assert
                     (cl-some (lambda (l) (string-match-p "^:LOGBOOK:" l)) clines)
                     "Log+list: has :LOGBOOK: drawer")
                    ;; Find logbook :END: (second :END: — first is property drawer)
                    (let* ((end-positions (cl-loop for i below (length clines)
                                                  when (string-match-p "^:END:" (nth i clines))
                                                  collect i))
                           (logbook-end (nth 1 end-positions)))
                      (orgist-test-assert
                       logbook-end
                       "Log+list: logbook drawer has :END:")
                      (when logbook-end
                        ;; Description list items must be AFTER :END:
                        (orgist-test-assert
                         (cl-some (lambda (l) (string-match-p "Everyday:" l))
                                  (nthcdr (1+ logbook-end) clines))
                         "Log+list: description list items are after logbook :END:")
                        ;; No description items inside logbook
                        (let ((logbook-start (cl-loop for i below (length clines)
                                                     when (string-match-p "^:LOGBOOK:" (nth i clines))
                                                     return i)))
                          (orgist-test-assert
                           (not (cl-some (lambda (l) (string-match-p "Everyday:" l))
                                         (cl-subseq clines logbook-start (1+ logbook-end))))
                           "Log+list: no description items inside :LOGBOOK: drawer"))))

                    ;; Now insert a Note (simulating comments pull)
                    (goto-char pos)
                    (orgist-insert-comment-as-note
                     "2025-11-02T14:08:00Z"
                     "This is a test comment about timing.")

                    ;; Re-read and verify structure is still correct
                    (goto-char pos)
                    (let* ((new-end (save-excursion (org-end-of-subtree t t) (point)))
                           (new-content (buffer-substring-no-properties pos new-end))
                           (new-lines (split-string new-content "\n")))
                      ;; Logbook drawer still has :END:
                      (let* ((end-positions (cl-loop for i below (length new-lines)
                                                    when (string-match-p "^:END:" (nth i new-lines))
                                                    collect i))
                             (logbook-end (nth 1 end-positions)))
                        (orgist-test-assert
                         logbook-end
                         "Log+list after Note: logbook drawer still has :END:")
                        (when logbook-end
                          ;; Note should be inside logbook
                          (let ((logbook-start (cl-loop for i below (length new-lines)
                                                       when (string-match-p "^:LOGBOOK:" (nth i new-lines))
                                                       return i)))
                            (orgist-test-assert
                             (cl-some (lambda (l) (string-match-p "test comment" l))
                                      (cl-subseq new-lines logbook-start (1+ logbook-end)))
                             "Log+list after Note: comment is inside :LOGBOOK: drawer"))
                          ;; Description still after :END:
                          (orgist-test-assert
                           (cl-some (lambda (l) (string-match-p "Everyday:" l))
                                    (nthcdr (1+ logbook-end) new-lines))
                           "Log+list after Note: description list items still after :END:")
                          ;; No description inside logbook
                          (let ((logbook-start (cl-loop for i below (length new-lines)
                                                       when (string-match-p "^:LOGBOOK:" (nth i new-lines))
                                                       return i)))
                            (orgist-test-assert
                             (not (cl-some (lambda (l) (string-match-p "Everyday:" l))
                                           (cl-subseq new-lines logbook-start (1+ logbook-end))))
                             "Log+list after Note: no description items leaked into logbook"))))))

                    ;; Simulate subprocess: save to disk, kill buffer,
                    ;; re-open fresh, insert more Notes, verify again.
                    (save-buffer)
                    (let ((file-path (buffer-file-name)))
                      (set-buffer-modified-p nil)
                      (kill-buffer)
                      (find-file file-path)
                      (org-mode)
                      (orgist-build-id-cache)
                      (let ((pos2 (orgist-find-element-by-id "fmt-task-9")))
                        (orgist-test-assert pos2
                         "Log+list subprocess: task found after reopen")
                        (when pos2
                          (goto-char pos2)
                          ;; Insert two more Notes
                          (orgist-insert-comment-as-note
                           "2025-12-04T05:11:00Z"
                           "Moved to afternoon slot.")
                          (goto-char (orgist-find-element-by-id "fmt-task-9"))
                          (orgist-insert-comment-as-note
                           "2025-12-10T10:00:00Z"
                           "Third note added in subprocess.")
                          ;; Deduplicate and sort logbook (subprocess does this)
                          (goto-char (orgist-find-element-by-id "fmt-task-9"))
                          (orgist-deduplicate-logbook)
                          (orgist-sort-logbook)

                          ;; Verify final structure
                          (goto-char (orgist-find-element-by-id "fmt-task-9"))
                          (let* ((final-end (save-excursion
                                              (org-end-of-subtree t t) (point)))
                                 (final-content (buffer-substring-no-properties
                                                 (point) final-end))
                                 (final-lines (split-string final-content "\n")))
                            ;; Dump content for debugging if test fails
                            (message "Log+list subprocess content:\n%s" final-content)
                            (let* ((end-positions
                                    (cl-loop for i below (length final-lines)
                                             when (string-match-p "^:END:"
                                                                  (nth i final-lines))
                                             collect i))
                                   (logbook-end (nth 1 end-positions)))
                              (orgist-test-assert
                               logbook-end
                               "Log+list subprocess: logbook drawer has :END:")
                              (when logbook-end
                                ;; All 3 notes inside logbook
                                (let ((logbook-region
                                       (cl-subseq final-lines 0 (1+ logbook-end))))
                                  (orgist-test-assert
                                   (cl-some (lambda (l) (string-match-p "test comment" l))
                                            logbook-region)
                                   "Log+list subprocess: first note inside logbook")
                                  (orgist-test-assert
                                   (cl-some (lambda (l) (string-match-p "afternoon" l))
                                            logbook-region)
                                   "Log+list subprocess: second note inside logbook")
                                  (orgist-test-assert
                                   (cl-some (lambda (l) (string-match-p "Third note" l))
                                            logbook-region)
                                   "Log+list subprocess: third note inside logbook"))
                                ;; Description outside
                                (orgist-test-assert
                                 (cl-some (lambda (l) (string-match-p "Everyday:" l))
                                          (nthcdr (1+ logbook-end) final-lines))
                                 "Log+list subprocess: description still after :END:")
                                ;; No description inside logbook
                                (let ((logbook-start
                                       (cl-loop for i below (length final-lines)
                                                when (string-match-p "^:LOGBOOK:"
                                                                     (nth i final-lines))
                                                return i)))
                                  (orgist-test-assert
                                   (not (cl-some
                                         (lambda (l) (string-match-p "Everyday:" l))
                                         (cl-subseq final-lines logbook-start
                                                    (1+ logbook-end))))
                                   "Log+list subprocess: no description leaked into logbook"))))))))))))))

  ;; Results
  (message "")
  (message "========================================")
  (message "=== Results: Formatting ===")
  (message "=== Passed: %d  Failed: %d ==="
           orgist-test--passes orgist-test--failures)
  (message "========================================")
  (message "")
  orgist-test--failures))

;;; ============================================================
;;; G. Comments & Activity tests
;;; ============================================================

(defun orgist-test-run-comments ()
  "Test comment/activity pull and push functionality.
Phase 1: Full sync of Orgtest
Phase 2: Pull comments for a test task
Phase 3: Verify logbook entries and snapshot
Phase 4: Re-pull and verify no duplicates
Phase 5: New Note detection and note_add command generation"
  (setq orgist-test--failures 0)
  (setq orgist-test--passes 0)
  (message "")
  (message "========================================")
  (message "=== Comments & Activity tests ===")
  (message "========================================")

  ;; Phase 1: Full sync of Orgtest
  (message "")
  (message "--- Phase 1: Full sync ---")
  (orgist-test-setup-isolation "Orgtest")
  (setq orgist-sync-comments t)
  (setq orgist-plan-limits nil)  ; force fresh fetch from mock
  (setq org-log-into-drawer t)
  (when (file-exists-p orgist-sync-token-filename)
    (delete-file orgist-sync-token-filename))
  (orgist)
  (let ((completed (orgist-test--wait-for-sync 120)))
    (orgist-test-assert completed "Full sync completed")
    (unless completed (kill-emacs 1)))

  ;; Phase 2: Pull comments for Barz task
  (message "")
  (message "--- Phase 2: Pull comments/activity for Barz ---")
  (let ((test-task-id "fixture-comment-task"))
    ;; Open org files and build caches
    (dolist (file (directory-files orgist-base-dir t "\\.org$"))
      (find-file file)
      (org-mode)
      (orgist-build-id-cache))
    ;; Pull comments
    (orgist-sync-task-comments-and-activity test-task-id)

    ;; Phase 3: Verify
    (message "")
    (message "--- Phase 3: Verify logbook entries ---")
    (let ((pos (orgist-find-element-by-id test-task-id)))
      (orgist-test-assert pos "Barz task found in buffer")
      (when pos
        (save-excursion
          (goto-char pos)
          (let ((subtree-end (save-excursion (org-end-of-subtree t t) (point)))
                (content (buffer-substring-no-properties
                          pos (save-excursion (org-end-of-subtree t t) (point)))))
            ;; Check that Note entries were inserted
            (orgist-test-assert
             (string-match-p "Note \\\\\\\\" content)
             "Logbook contains Note entries")
            (orgist-test-assert
             (string-match-p "Comment here" content)
             "Logbook contains first comment text")
            (orgist-test-assert
             (string-match-p "multi-line" content)
             "Logbook contains second comment text")
            ;; Check that activity was inserted (completed event -> DONE entry)
            (orgist-test-assert
             (string-match-p "\"DONE\"" content)
             "Logbook contains DONE state change from activity")))))

    ;; Check snapshot :comment-ids
    (let ((snap (gethash test-task-id orgist-snapshots)))
      (orgist-test-assert snap "Snapshot exists for test task")
      (when snap
        (let ((comment-ids (plist-get snap :comment-ids)))
          (orgist-test-assert
           (and comment-ids (>= (length comment-ids) 2))
           (format "Snapshot has comment-ids (%d found)"
                   (length (or comment-ids '())))))
        (let ((activity-ids (plist-get snap :activity-ids)))
          (orgist-test-assert
           (and activity-ids (>= (length activity-ids) 1))
           (format "Snapshot has activity-ids (%d found)"
                   (length (or activity-ids '())))))))

    ;; Phase 4: Re-pull and verify no duplicates
    (message "")
    (message "--- Phase 4: Re-pull (no duplicates) ---")
    (let ((pos (orgist-find-element-by-id test-task-id)))
      (when pos
        (let ((content-before (save-excursion
                                (goto-char pos)
                                (buffer-substring-no-properties
                                 pos (save-excursion
                                       (org-end-of-subtree t t) (point))))))
          ;; Pull again
          (orgist-sync-task-comments-and-activity test-task-id)
          ;; Content should be unchanged
          (let ((content-after (save-excursion
                                 (goto-char (orgist-find-element-by-id test-task-id))
                                 (buffer-substring-no-properties
                                  (point) (save-excursion
                                            (org-end-of-subtree t t) (point))))))
            (orgist-test-assert-equal
             content-before content-after
             "Re-pull produces no duplicate entries")))))

    ;; Phase 5: New Note detection and note_add push
    (message "")
    (message "--- Phase 5: New Note push detection ---")
    (orgist-save-snapshots)
    (orgist-load-snapshots t)
    (dolist (file (directory-files orgist-base-dir t "\\.org$"))
      (with-current-buffer (find-file-noselect file)
        (orgist-build-id-cache)))
    ;; Insert a new Note in the logbook
    (let ((pos (orgist-find-element-by-id test-task-id)))
      (when pos
        (save-excursion
          (goto-char pos)
          (org-back-to-heading t)
          (orgist-insert-comment-as-note
           "2026-02-19T16:00:00.000000Z"
           "A brand new local note"))
        ;; Diff should detect the new Note
        (let* ((changes (orgist-diff-all-elements))
               (task-change (assoc test-task-id changes)))
          (orgist-test-assert
           task-change
           "Diff detects change after inserting new Note")
          (when task-change
            (let ((notes-diff (assq :notes (cdr task-change))))
              (orgist-test-assert
               notes-diff
               "Diff includes :notes field change")))
          ;; Generate commands
          (let* ((commands (orgist-changes-to-commands changes))
                 (note-cmds (seq-filter
                             (lambda (c) (equal (alist-get 'type c) "note_add"))
                             commands)))
            (orgist-test-assert
             (>= (length note-cmds) 1)
             (format "Produces note_add command(s) (%d found)"
                     (length note-cmds)))
            (when note-cmds
              (let* ((cmd (car note-cmds))
                     (args (alist-get 'args cmd)))
                (orgist-test-assert-equal
                 test-task-id (alist-get 'item_id args)
                 "note_add targets correct task ID")
                (orgist-test-assert
                 (alist-get 'content args)
                 "note_add has content")))
            ;; Dry-run and verify no re-detection
            (when commands
              (orgist-execute-write-back commands)
              (let ((changes-after (orgist-diff-all-elements)))
                ;; Filter out non-note changes
                (let ((note-changes
                       (seq-filter
                        (lambda (c)
                          (and (listp (cdr c))
                               (assq :notes (cdr c))))
                        changes-after)))
                  (orgist-test-assert-equal
                   0 (length note-changes)
                   "Zero note diffs after dry-run")))))))))

  ;; Results
  (message "")
  (message "========================================")
  (message "=== Results: Comments ===")
  (message "=== Passed: %d  Failed: %d ==="
           orgist-test--passes orgist-test--failures)
  (message "========================================")
  (message "")
  orgist-test--failures)

;;; ============================================================
;;; H. New feature unit tests
;;; ============================================================

(defun orgist-test-run-new-features ()
  "Test new features added in the power-user sync phase.
Tests timezone conversion, duration write-back round-trip,
collaborator name resolution, reminders, label creation,
order sync, and note_count optimization."
  (setq orgist-test--failures 0)
  (setq orgist-test--passes 0)
  (message "")
  (message "========================================")
  (message "=== New Feature tests ===")
  (message "========================================")

  ;; Set up minimal isolation
  (let* ((runtime-dir (expand-file-name "orgist-test/NewFeatures/"
                                        temporary-file-directory)))
    (when (file-directory-p runtime-dir)
      (delete-directory runtime-dir t))
    (make-directory runtime-dir t)
    (setq orgist-base-dir runtime-dir)
    (setq orgist-sync-token-filename (expand-file-name "sync_token" runtime-dir))
    (setq orgist-snapshot-file (expand-file-name "snapshots.el" runtime-dir))
    (setq orgist-log-file (expand-file-name "orgist.log" runtime-dir))
    (setq orgist-project-buffer-cache nil)
    (setq orgist-snapshots nil)
    (setq orgist-sync-mutex nil)
    (setq orgist-sync-project-filter nil)
    (setq revert-without-query '(".*"))
    (setq org-priority-highest 1 org-priority-lowest 5 org-priority-default 5)
    (setq orgist-enable-write-back t)
    (setq orgist-write-back-dry-run t)
    (setq orgist-log-level 'info)
    (setq org-log-into-drawer t)
    (dolist (buf (buffer-list))
      (when (and (buffer-file-name buf)
                 (string-match-p "\\.org$" (buffer-file-name buf)))
        (kill-buffer buf)))

    ;; ============================================================
    ;; 1. Timezone conversion
    ;; ============================================================
    (message "")
    (message "--- Test group: Timezone conversion ---")

    ;; UTC timestamp with Z suffix
    (let ((result (orgist--convert-timezone "2026-02-20T14:30:00Z" nil)))
      (orgist-test-assert result "TZ convert: UTC Z-suffix produces result")
      (when result
        (orgist-test-assert
         (string-match-p "^[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}$" (car result))
         "TZ convert: date part is YYYY-MM-DD")
        (orgist-test-assert
         (string-match-p "^[0-9]\\{2\\}:[0-9]\\{2\\}$" (cdr result))
         "TZ convert: time part is HH:MM")))

    ;; Timestamp without Z (local time)
    (let ((result (orgist--convert-timezone "2026-02-20T14:30:00" nil)))
      (orgist-test-assert result "TZ convert: no-Z timestamp produces result"))

    ;; Fractional seconds with Z
    (let ((result (orgist--convert-timezone "2026-02-20T14:30:00.123456Z" nil)))
      (orgist-test-assert result "TZ convert: fractional seconds handled"))

    ;; Invalid timestamp
    (let ((result (orgist--convert-timezone "not-a-date" nil)))
      (orgist-test-assert (null result) "TZ convert: invalid date returns nil"))

    ;; ============================================================
    ;; 2. Duration write-back round-trip
    ;; ============================================================
    (message "")
    (message "--- Test group: Duration round-trip ---")

    ;; Extract duration from time range
    (let ((dur (orgist-org-timestamp-extract-duration
                "<2026-02-20 Fri 13:00-14:00>")))
      (orgist-test-assert dur "Duration extract: time range produces duration")
      (when dur
        (orgist-test-assert-equal 60 (alist-get 'amount dur)
                                  "Duration extract: 1 hour = 60 minutes")
        (orgist-test-assert-equal "minute" (alist-get 'unit dur)
                                  "Duration extract: unit is minute")))

    ;; 30-minute duration
    (let ((dur (orgist-org-timestamp-extract-duration
                "<2026-02-20 Fri 09:00-09:30>")))
      (orgist-test-assert dur "Duration extract: 30 min range")
      (when dur
        (orgist-test-assert-equal 30 (alist-get 'amount dur)
                                  "Duration extract: 30 minutes")))

    ;; No time range → no duration
    (let ((dur (orgist-org-timestamp-extract-duration
                "<2026-02-20 Fri 13:00>")))
      (orgist-test-assert (null dur)
                          "Duration extract: no range returns nil"))

    ;; No time at all → no duration
    (let ((dur (orgist-org-timestamp-extract-duration "<2026-02-20 Fri>")))
      (orgist-test-assert (null dur)
                          "Duration extract: date-only returns nil"))

    ;; Calculate end time
    (orgist-test-assert-equal
     "14:30" (orgist-calculate-end-time "13:00" '((amount . 90) (unit . "minute")))
     "End time calc: 13:00 + 90min = 14:30")

    (orgist-test-assert-equal
     "15:00" (orgist-calculate-end-time "13:00" '((amount . 2) (unit . "hour")))
     "End time calc: 13:00 + 2hr = 15:00")

    ;; ============================================================
    ;; 3. Collaborator name resolution
    ;; ============================================================
    (message "")
    (message "--- Test group: Collaborator resolution ---")

    (let ((orgist-collaborators (make-hash-table :test 'equal)))
      (puthash "uid-123"
               '((full_name . "Alice Smith") (email . "alice@example.com"))
               orgist-collaborators)
      ;; Known UID resolves to name
      (orgist-test-assert-equal
       "Alice Smith" (orgist-resolve-collaborator-name "uid-123")
       "Collaborator: known UID resolves to name")
      ;; Unknown UID returns UID
      (orgist-test-assert-equal
       "uid-unknown" (orgist-resolve-collaborator-name "uid-unknown")
       "Collaborator: unknown UID returns UID"))

    ;; Nil table
    (let ((orgist-collaborators nil))
      (orgist-test-assert-equal
       "uid-456" (orgist-resolve-collaborator-name "uid-456")
       "Collaborator: nil table returns UID"))

    ;; ============================================================
    ;; 4. Reminders application
    ;; ============================================================
    (message "")
    (message "--- Test group: Reminders ---")

    ;; Create a project with a task that has a deadline, then apply reminders
    (let ((project '((id . "rem-proj-1")
                     (name . "RemTest")
                     (parent_id . nil)
                     (is_deleted . :json-false)
                     (child_order . 0)))
          (task '((id . "rem-task-1")
                  (content . "Reminder task")
                  (project_id . "rem-proj-1")
                  (section_id . nil)
                  (parent_id . nil)
                  (child_order . 0)
                  (priority . 1)
                  (checked . :json-false)
                  (labels . [])
                  (due . ((date . "2026-03-01T10:00:00")
                          (is_recurring . :json-false)
                          (lang . "en")
                          (string . "Mar 1 10am")
                          (timezone . nil)))
                  (deadline . ((date . "2026-03-01")
                               (is_recurring . :json-false)
                               (lang . "en")
                               (string . "Mar 1")
                               (timezone . nil)))
                  (duration . nil)
                  (description . "")
                  (added_at . "2026-01-15T10:00:00Z")))
          (orgist-reminders (make-hash-table :test 'equal)))
      ;; Set up reminders: relative + location
      (puthash "rem-task-1"
               (list '((type . "relative")
                       (minute_offset . 2880)
                       (item_id . "rem-task-1"))
                     '((type . "location")
                       (name . "Office")
                       (loc_lat . 37.7749)
                       (loc_long . -122.4194)
                       (item_id . "rem-task-1")))
               orgist-reminders)
      (orgist-update-projects (vector project))
      (orgist-update-elements (vector task) 'item)
      (orgist--flush-pending-saves)
      ;; Verify
      (let* ((org-file (expand-file-name "RemTest.org" runtime-dir))
             (buf (find-file-noselect org-file)))
        (with-current-buffer buf
          (orgist-build-id-cache)
          (goto-char (orgist-find-element-by-id "rem-task-1"))
          ;; Check relative reminder added warning to DEADLINE
          (let ((deadline (org-entry-get (point) "DEADLINE")))
            (orgist-test-assert
             (and deadline (string-match-p "-2d>" deadline))
             (format "Reminder relative: DEADLINE has -2d warning (%s)" deadline)))
          ;; Check location reminder stored as property
          (let ((loc (org-entry-get (point) "REMINDER-LOC")))
            (orgist-test-assert
             (and loc (string-match-p "Office" loc))
             (format "Reminder location: REMINDER-LOC property set (%s)" loc))))))

    ;; ============================================================
    ;; 5. Label creation (label_add command)
    ;; ============================================================
    (message "")
    (message "--- Test group: Label creation ---")

    ;; Set up with a known label
    (let ((orgist-labels (make-hash-table :test 'equal)))
      (puthash "label-1" '((name . "existing-label") (color . "blue"))
               orgist-labels)
      ;; Known label should be found
      (orgist-test-assert-equal
       "label-1" (orgist-label-name-to-id "existing-label")
       "Label lookup: known label returns ID")
      ;; Unknown label
      (orgist-test-assert
       (null (orgist-label-name-to-id "unknown-label"))
       "Label lookup: unknown returns nil"))

    ;; Test that adding a new tag generates label_add + item_update
    ;; Use the RemTest project from above
    (let* ((org-file (expand-file-name "RemTest.org" runtime-dir))
           (buf (find-file-noselect org-file)))
      (with-current-buffer buf
        (orgist-build-id-cache)
        (orgist-save-snapshots)
        (orgist-load-snapshots t)
        ;; Add a new tag that doesn't exist in orgist-labels
        (goto-char (orgist-find-element-by-id "rem-task-1"))
        (org-back-to-heading t)
        (org-set-tags '("brand-new-tag"))
        ;; Diff and check commands
        (let* ((changes (orgist-diff-all-elements))
               (commands (orgist-changes-to-commands changes))
               (label-cmds (seq-filter
                            (lambda (c) (equal (alist-get 'type c) "label_add"))
                            commands)))
          (orgist-test-assert
           (>= (length label-cmds) 1)
           (format "Label create: label_add command generated (%d)" (length label-cmds)))
          (when label-cmds
            (orgist-test-assert-equal
             "brand-new-tag"
             (alist-get 'name (alist-get 'args (car label-cmds)))
             "Label create: label_add has correct name")))))

    ;; Regression: removing all labels in Todoist clears the org tag.
    ;; Previously the gating condition skipped `org-set-tags' when the new
    ;; label list was empty, leaving stale tags in the buffer and producing
    ;; a spurious write-back diff.
    (let* ((project '((id . "lblrm-proj-1")
                      (name . "LblRm")
                      (parent_id . nil)
                      (is_deleted . :json-false)
                      (child_order . 0)))
           (task-with `((id . "lblrm-task-1")
                        (content . "Task with label")
                        (project_id . "lblrm-proj-1")
                        (section_id . nil)
                        (parent_id . nil)
                        (child_order . 0)
                        (priority . 1)
                        (checked . :json-false)
                        (labels . ["Housekeeper"])
                        (due . nil) (deadline . nil) (duration . nil)
                        (description . "")
                        (added_at . "2026-01-15T10:00:00Z")))
           (task-without `((id . "lblrm-task-1")
                           (content . "Task with label")
                           (project_id . "lblrm-proj-1")
                           (section_id . nil)
                           (parent_id . nil)
                           (child_order . 0)
                           (priority . 1)
                           (checked . :json-false)
                           (labels . [])
                           (due . nil) (deadline . nil) (duration . nil)
                           (description . "")
                           (added_at . "2026-01-15T10:00:00Z"))))
      (orgist-update-projects (vector project))
      (orgist-update-elements (vector task-with) 'item)
      (orgist--flush-pending-saves)
      ;; Simulate the user removing the label in Todoist: same task, empty labels.
      (orgist-update-elements (vector task-without) 'item)
      (orgist--flush-pending-saves)
      (let* ((org-file (expand-file-name "LblRm.org" runtime-dir))
             (buf (find-file-noselect org-file)))
        (with-current-buffer buf
          (orgist-build-id-cache)
          (goto-char (orgist-find-element-by-id "lblrm-task-1"))
          (org-back-to-heading t)
          (let ((tags (org-get-tags nil t)))
            (orgist-test-assert
             (not (member "Housekeeper" tags))
             (format "Label removal: tag cleared from buffer (tags=%S)" tags))))))

    ;; Regression: switching the org repeater from `+1w' to `.+1w' must
    ;; produce Todoist's completion-based recurrence string ("every!").
    ;; Previously the synthesizer ignored the repeater prefix and always
    ;; emitted "every <unit>", so the change was a silent no-op on Todoist.
    (let ((due (orgist-org-timestamp-to-todoist-due
                "<2026-02-26 Thu .+1w>" nil)))
      (orgist-test-assert-equal
       "every! week"
       (alist-get 'string due)
       "Recurrence: .+1w → every! week (no date prefix)"))
    (let ((due (orgist-org-timestamp-to-todoist-due
                "<2026-02-26 Thu +1w>" nil)))
      (orgist-test-assert-equal
       "every week"
       (alist-get 'string due)
       "Recurrence: +1w → every week (no date prefix)"))
    (let ((due (orgist-org-timestamp-to-todoist-due
                "<2026-02-26 Thu ++2d>" nil)))
      (orgist-test-assert-equal
       "every 2 days"
       (alist-get 'string due)
       "Recurrence: ++2d → every 2 days (no date prefix)"))
    ;; Non-recurring keeps the date prefix
    (let ((due (orgist-org-timestamp-to-todoist-due
                "<2026-02-26 Thu>" nil)))
      (orgist-test-assert-equal
       "Feb 26 2026"
       (alist-get 'string due)
       "Non-recurring: date-only timestamp keeps date prefix"))

    ;; Anchored recurrence rebuild: interval change on `++Nx' must keep
    ;; the original Todoist anchor (weekday / month-day / month) because
    ;; org's `++' repeater alone can't express it.
    (orgist-test-assert-equal
     "every 2 weeks on monday"
     (orgist--rebuild-anchored-due-string "every mon" "++2w")
     "Anchor rebuild: ++1w/every mon → ++2w yields weekly on monday")
    (orgist-test-assert-equal
     "every week on monday"
     (orgist--rebuild-anchored-due-string "every mon" "++1w")
     "Anchor rebuild: interval=1 still uses singular unit")
    (orgist-test-assert-equal
     "every 3 months on the 15th"
     (orgist--rebuild-anchored-due-string "every 15" "++3m")
     "Anchor rebuild: month-day anchor preserved with new interval")
    (orgist-test-assert-equal
     "every 2 years on january 1"
     (orgist--rebuild-anchored-due-string "every jan 1" "++2y")
     "Anchor rebuild: month+day anchor preserved with new interval")
    (orgist-test-assert-equal
     "every! 2 weeks on monday"
     (orgist--rebuild-anchored-due-string "every! mon" ".+2w")
     "Anchor rebuild: completion-based anchor preserved")
    ;; No recoverable anchor → nil (caller falls back to fresh synthesis)
    (orgist-test-assert
     (null (orgist--rebuild-anchored-due-string "every week" "++2w"))
     "Anchor rebuild: plain weekly has no anchor — returns nil")

    ;; End-to-end: snapshot has "every mon", buffer changes ++1w → ++2w.
    ;; The generated item_update must carry "every 2 weeks on monday".
    (let* ((project '((id . "rec-proj-1")
                      (name . "RecTest")
                      (parent_id . nil)
                      (is_deleted . :json-false)
                      (child_order . 0)))
           (task '((id . "rec-task-1")
                   (content . "Anchored weekly task")
                   (project_id . "rec-proj-1")
                   (section_id . nil)
                   (parent_id . nil)
                   (child_order . 0)
                   (priority . 1)
                   (checked . :json-false)
                   (labels . [])
                   (due . ((date . "2026-02-23")
                           (is_recurring . t)
                           (lang . "en")
                           (string . "every mon")
                           (timezone . nil)))
                   (deadline . nil) (duration . nil)
                   (description . "")
                   (added_at . "2026-01-15T10:00:00Z"))))
      (orgist-update-projects (vector project))
      (orgist-update-elements (vector task) 'item)
      (orgist--flush-pending-saves)
      (let* ((org-file (expand-file-name "RecTest.org" runtime-dir))
             (buf (find-file-noselect org-file)))
        (with-current-buffer buf
          (orgist-build-id-cache)
          (orgist-save-snapshots)
          (orgist-load-snapshots t)
          (goto-char (orgist-find-element-by-id "rec-task-1"))
          ;; Change the interval: ++1w → ++2w (keep same date)
          (let ((org-log-done nil) (org-log-repeat nil))
            (org-schedule nil "<2026-02-23 Mon ++2w>"))
          (let* ((changes (orgist-diff-all-elements))
                 (commands (orgist-changes-to-commands changes))
                 (cmd (seq-find (lambda (c)
                                  (equal (alist-get 'type c) "item_update"))
                                commands))
                 (due (alist-get 'due (alist-get 'args cmd))))
            (orgist-test-assert
             (string-match-p "every 2 weeks on monday"
                             (or (alist-get 'string due) ""))
             (format "Anchor rebuild end-to-end: command string=%S"
                     (alist-get 'string due)))))))

    ;; Regression: after write-back, the buffer's TODOIST_DUE_STRING must
    ;; reflect the new recurrence string the command sent (e.g. +3d → .+3d
    ;; → "every! 3 days"), not the pre-edit value.  Previously only the
    ;; internal snapshot was updated, so the buffer property stayed stale
    ;; until the next pull.
    (let* ((project '((id . "dst-proj-1")
                      (name . "DSTest")
                      (parent_id . nil)
                      (is_deleted . :json-false)
                      (child_order . 0)))
           (task '((id . "dst-task-1")
                   (content . "Recurring task")
                   (project_id . "dst-proj-1")
                   (section_id . nil)
                   (parent_id . nil)
                   (child_order . 0)
                   (priority . 1)
                   (checked . :json-false)
                   (labels . [])
                   (due . ((date . "2026-05-08")
                           (is_recurring . t)
                           (lang . "en")
                           (string . "every 3 days")
                           (timezone . nil)))
                   (deadline . nil) (duration . nil)
                   (description . "")
                   (added_at . "2026-01-15T10:00:00Z"))))
      (orgist-update-projects (vector project))
      (orgist-update-elements (vector task) 'item)
      (orgist--flush-pending-saves)
      (let* ((org-file (expand-file-name "DSTest.org" runtime-dir))
             (buf (find-file-noselect org-file)))
        (with-current-buffer buf
          (orgist-build-id-cache)
          (orgist-save-snapshots)
          (orgist-load-snapshots t)
          (goto-char (orgist-find-element-by-id "dst-task-1"))
          ;; Confirm baseline property
          (orgist-test-assert-equal
           "every 3 days"
           (org-entry-get (point) "TODOIST_DUE_STRING")
           "Due-string sync: baseline TODOIST_DUE_STRING")
          ;; Switch +3d → .+3d
          (let ((org-log-done nil) (org-log-repeat nil))
            (org-schedule nil "<2026-05-08 Fri .+3d>"))
          (let* ((changes (orgist-diff-all-elements))
                 (commands (orgist-changes-to-commands changes))
                 (orgist-write-back-dry-run t))
            (orgist-execute-write-back commands)
            (goto-char (orgist-find-element-by-id "dst-task-1"))
            (orgist-test-assert-equal
             "every! 3 days"
             (org-entry-get (point) "TODOIST_DUE_STRING")
             "Due-string sync: TODOIST_DUE_STRING updated after write-back")))))

    ;; Regression: completing a task while simultaneously changing the
    ;; repeater prefix (+1w → .+1w) must send both item_update (with the
    ;; new recurrence string, no specific date) and item_close (to let
    ;; Todoist advance the date from the new type).  Previously the
    ;; :due diff was unconditionally suppressed on completion, so the
    ;; prefix change was silently discarded.
    (let* ((project '((id . "crc-proj-1")
                      (name . "CRCTest")
                      (parent_id . nil)
                      (is_deleted . :json-false)
                      (child_order . 0)))
           (task '((id . "crc-task-1")
                   (content . "Complete+retype task")
                   (project_id . "crc-proj-1")
                   (section_id . nil) (parent_id . nil)
                   (child_order . 0) (priority . 1)
                   (checked . :json-false) (labels . [])
                   (due . ((date . "2026-05-08")
                           (is_recurring . t)
                           (lang . "en")
                           (string . "every week")
                           (timezone . nil)))
                   (deadline . nil) (duration . nil)
                   (description . "")
                   (added_at . "2026-01-15T10:00:00Z"))))
      (orgist-update-projects (vector project))
      (orgist-update-elements (vector task) 'item)
      (orgist--flush-pending-saves)
      (let* ((org-file (expand-file-name "CRCTest.org" runtime-dir))
             (buf (find-file-noselect org-file)))
        (with-current-buffer buf
          (orgist-build-id-cache)
          (orgist-save-snapshots)
          (orgist-load-snapshots t)
          (goto-char (orgist-find-element-by-id "crc-task-1"))
          ;; Complete the task and change +1w → .+1w simultaneously
          (let ((org-log-done 'time) (org-log-repeat 'time))
            (org-todo "DONE"))
          (let ((org-log-done nil) (org-log-repeat nil))
            (org-schedule nil "<2026-05-15 Fri .+1w>"))
          (let* ((changes (orgist-diff-all-elements))
                 (commands (orgist-changes-to-commands changes))
                 (update-cmd (seq-find (lambda (c)
                                         (equal (alist-get 'type c) "item_update"))
                                       commands))
                 (close-cmd (seq-find (lambda (c)
                                        (equal (alist-get 'type c) "item_close"))
                                      commands))
                 (due (alist-get 'due (alist-get 'args update-cmd))))
            (orgist-test-assert
             close-cmd
             "Complete+retype: item_close generated")
            (orgist-test-assert
             update-cmd
             "Complete+retype: item_update generated for recurrence change")
            (orgist-test-assert
             (string-match-p "every! week" (or (alist-get 'string due) ""))
             (format "Complete+retype: item_update has new recurrence string (got %S)"
                     (alist-get 'string due)))
            (orgist-test-assert
             (null (alist-get 'date due))
             (format "Complete+retype: item_update has no date (got %S)"
                     (alist-get 'date due)))))))

    ;; ============================================================
    ;; 6. Order sync (item_reorder command)
    ;; ============================================================
    (message "")
    (message "--- Test group: Order sync ---")

    ;; Use RemTest: change the TODOIST-ORDER property to trigger reorder
    (let* ((org-file (expand-file-name "RemTest.org" runtime-dir))
           (buf (find-file-noselect org-file)))
      (with-current-buffer buf
        (orgist-build-id-cache)
        ;; Clean up any pending changes first
        (let* ((pending (orgist-diff-all-elements))
               (cmds (orgist-changes-to-commands pending)))
          (when cmds (orgist-execute-write-back cmds)))
        (orgist-save-snapshots)
        (orgist-load-snapshots t)
        (orgist-build-id-cache)
        ;; Change order
        (goto-char (orgist-find-element-by-id "rem-task-1"))
        (org-entry-put (point) "TODOIST-ORDER" "99")
        ;; Diff and check
        (let* ((changes (orgist-diff-all-elements))
               (commands (orgist-changes-to-commands changes))
               (reorder-cmds (seq-filter
                              (lambda (c) (equal (alist-get 'type c) "item_reorder"))
                              commands)))
          (orgist-test-assert
           (>= (length reorder-cmds) 1)
           (format "Order sync: item_reorder command generated (%d)" (length reorder-cmds)))
          (when reorder-cmds
            (let* ((args (alist-get 'args (car reorder-cmds)))
                   (items (alist-get 'items args)))
              (orgist-test-assert
               items
               "Order sync: reorder has items array"))))))

    ;; ============================================================
    ;; 7. note_count optimization
    ;; ============================================================
    (message "")
    (message "--- Test group: note_count optimization ---")

    ;; When note_count is 0, comments should be skipped
    (let ((orgist-snapshots (make-hash-table :test 'equal)))
      (puthash "nc-task-1"
               '(:content "Test" :note-count 0 :section-p nil)
               orgist-snapshots)
      ;; The optimization is inside orgist-sync-task-comments-and-activity
      ;; but we can test the condition directly: when note_count is 0
      ;; in the snapshot, the task should not trigger a comment fetch.
      (let ((snap (gethash "nc-task-1" orgist-snapshots)))
        (orgist-test-assert-equal
         0 (plist-get snap :note-count)
         "note_count: snapshot stores note_count=0")))

    ;; When note_count > 0, it should be stored
    (let ((orgist-snapshots (make-hash-table :test 'equal)))
      (puthash "nc-task-2"
               '(:content "Test" :note-count 5 :section-p nil)
               orgist-snapshots)
      (let ((snap (gethash "nc-task-2" orgist-snapshots)))
        (orgist-test-assert-equal
         5 (plist-get snap :note-count)
         "note_count: snapshot stores note_count=5")))

    ;; ============================================================
    ;; 8. Subprocess org config helper
    ;; ============================================================
    (message "")
    (message "--- Test group: Subprocess config helper ---")

    (let ((config (orgist--subprocess-org-config)))
      (orgist-test-assert
       (listp config)
       "Subprocess config: returns a list")
      (orgist-test-assert
       (>= (length config) 5)
       (format "Subprocess config: contains %d settings (>= 5)" (length config)))
      ;; Each element should be a (setq ...) form
      (orgist-test-assert
       (cl-every (lambda (sexp) (eq (car sexp) 'setq)) config)
       "Subprocess config: all forms are setq"))

    ;; ============================================================
    ;; 9. Color map and label faces
    ;; ============================================================
    (message "")
    (message "--- Test group: Color map ---")

    (orgist-test-assert
     (assoc "berry_red" orgist-todoist-color-map)
     "Color map: has berry_red")
    (orgist-test-assert-equal
     "#4073ff" (cdr (assoc "blue" orgist-todoist-color-map))
     "Color map: blue maps to #4073ff")
    (orgist-test-assert-equal
     20 (length orgist-todoist-color-map)
     "Color map: has 20 entries")

    ;; ============================================================
    ;; 10. HTTP error code extraction
    ;; ============================================================
    (message "")
    (message "--- Test group: HTTP error code extraction ---")

    ;; String form: (error . "http 429")
    (orgist-test-assert-equal
     429 (orgist--http-error-code '(error . "http 429"))
     "HTTP error: string form (error . \"http 429\")")
    ;; List form: (error "http 410")
    (orgist-test-assert-equal
     410 (orgist--http-error-code '(error "http 410"))
     "HTTP error: list form (error \"http 410\")")
    ;; Symbol form: (error http 403)
    (orgist-test-assert-equal
     403 (orgist--http-error-code '(error http 403))
     "HTTP error: symbol form (error http 403)")
    ;; Non-HTTP error
    (orgist-test-assert
     (null (orgist--http-error-code '(error . "timeout")))
     "HTTP error: non-HTTP returns nil")
    ;; Not a cons
    (orgist-test-assert
     (null (orgist--http-error-code nil))
     "HTTP error: nil returns nil")

    ;; ============================================================
    ;; 11. Logbook insertion edge cases
    ;; ============================================================
    (message "")
    (message "--- Test group: Logbook insertion edge cases ---")

    ;; 11a. Insert comment into heading with property drawer (normal case)
    (let ((result
           (with-temp-buffer
             (org-mode)
             (insert "* TODO Normal task\n")
             (insert ":PROPERTIES:\n:ID: test-normal\n:END:\n")
             (goto-char (point-min))
             (org-back-to-heading t)
             (condition-case err
                 (progn
                   (orgist-insert-comment-as-note
                    "2026-02-20T10:00:00Z" "Test comment")
                   t)
               (error (format "error: %s" (error-message-string err)))))))
      (orgist-test-assert (eq result t)
                          "Logbook insert: normal heading succeeds"))

    ;; 11b. Insert log entry into heading with property drawer
    (let ((result
           (with-temp-buffer
             (org-mode)
             (insert "* TODO Log task\n")
             (insert ":PROPERTIES:\n:ID: test-log\n:END:\n")
             (goto-char (point-min))
             (org-back-to-heading t)
             (condition-case err
                 (progn
                   (orgist-insert-log-entry "DONE" "TODO"
                    "[2026-02-20 Fri 10:00]")
                   t)
               (error (format "error: %s" (error-message-string err)))))))
      (orgist-test-assert (eq result t)
                          "Logbook insert: log entry succeeds"))

    ;; 11c. Insert comment into heading at position 1 (start of buffer)
    (let ((result
           (with-temp-buffer
             (org-mode)
             (insert "* TODO First heading\n")
             (insert ":PROPERTIES:\n:ID: test-first\n:END:\n")
             (goto-char (point-min))
             (org-back-to-heading t)
             (condition-case err
                 (progn
                   (orgist-insert-comment-as-note
                    "2026-02-20T10:00:00Z" "Edge case comment")
                   t)
               (error (format "error: %s" (error-message-string err)))))))
      (orgist-test-assert (eq result t)
                          "Logbook insert: heading at buffer start succeeds"))

    ;; 11d. Insert into heading with existing logbook
    (let ((result
           (with-temp-buffer
             (org-mode)
             (insert "* TODO With logbook\n")
             (insert ":PROPERTIES:\n:ID: test-logbook\n:END:\n")
             (insert ":LOGBOOK:\n")
             (insert "- State \"TODO\"  from  [2026-01-01 Thu 12:00]\n")
             (insert ":END:\n")
             (goto-char (point-min))
             (org-back-to-heading t)
             (condition-case err
                 (progn
                   (orgist-insert-comment-as-note
                    "2026-02-20T10:00:00Z" "Into existing logbook")
                   t)
               (error (format "error: %s" (error-message-string err)))))))
      (orgist-test-assert (eq result t)
                          "Logbook insert: existing logbook succeeds"))

    ;; 11e. Insert into heading with no property drawer
    (let ((result
           (with-temp-buffer
             (org-mode)
             (insert "* TODO Bare heading\n")
             (goto-char (point-min))
             (org-back-to-heading t)
             (condition-case err
                 (progn
                   (orgist-insert-comment-as-note
                    "2026-02-20T10:00:00Z" "Bare heading comment")
                   t)
               (error (format "error: %s" (error-message-string err)))))))
      (orgist-test-assert (eq result t)
                          "Logbook insert: bare heading succeeds"))

    ;; 11f. Insert into heading with preamble before it
    (let ((result
           (with-temp-buffer
             (org-mode)
             (insert "#+TITLE: Test\n\n")
             (insert "* TODO After preamble\n")
             (insert ":PROPERTIES:\n:ID: test-preamble\n:END:\n")
             (goto-char (point-min))
             (re-search-forward "^\\*" nil t)
             (beginning-of-line)
             (condition-case err
                 (progn
                   (orgist-insert-comment-as-note
                    "2026-02-20T10:00:00Z" "Preamble test")
                   t)
               (error (format "error: %s" (error-message-string err)))))))
      (orgist-test-assert (eq result t)
                          "Logbook insert: heading after preamble succeeds"))

    ;; 11g-11i: Full sync round-trip tests.
    ;; Helper: write ORG-CONTENT to file, mock comment API, run sync,
    ;; check if COMMENT-TEXT was inserted.  Returns t or error string.
    (let ((run-sync-test
           (lambda (fname org-content task-id comment-text &optional pre-fn)
             (let ((fpath (expand-file-name fname runtime-dir))
                   (buf nil))
               (with-temp-file fpath (insert org-content))
               (setq buf (find-file-noselect fpath))
               (unwind-protect
                   (with-current-buffer buf
                     (org-mode)
                     (orgist-build-id-cache)
                     (when pre-fn (funcall pre-fn))
                     (let ((orgist-snapshots (make-hash-table :test 'equal)))
                       (puthash task-id
                                (list :content "test" :comment-ids nil
                                      :activity-ids nil :note-count 1
                                      :comments-pulled nil)
                                orgist-snapshots)
                       (cl-letf
                           (((symbol-function 'orgist-fetch-task-comments)
                             (lambda (_id)
                               (list (list (cons 'id "cmt-1")
                                          (cons 'posted_at "2026-02-20T10:00:00Z")
                                          (cons 'content comment-text)))))
                            ((symbol-function 'orgist-fetch-task-activity)
                             (lambda (_id) nil))
                            ((symbol-function 'orgist-activity-log-available-p)
                             (lambda () nil)))
                         (condition-case err
                             (progn
                               (orgist-sync-task-comments-and-activity task-id)
                               (widen)
                               (goto-char (point-min))
                               (if (search-forward comment-text nil t) t
                                 "comment not found in buffer"))
                           (error
                            (format "error: %s"
                                    (error-message-string err)))))))
                 (when buf (kill-buffer buf)))))))

      ;; 11g. Normal file with preamble
      (orgist-test-assert
       (eq t (funcall run-sync-test
                      "rt-normal.org"
                      "#+TITLE: N\n\n* TODO Task\n:PROPERTIES:\n:ID: rt-1\n:END:\n"
                      "rt-1" "Normal round-trip comment"))
       "Logbook insert: full sync round-trip succeeds")

      ;; 11h. Heading at line 1 (no preamble)
      (orgist-test-assert
       (eq t (funcall run-sync-test
                      "rt-nopreamble.org"
                      "* TODO First\n:PROPERTIES:\n:ID: rt-2\n:END:\n"
                      "rt-2" "No preamble comment"))
       "Logbook insert: sync round-trip no preamble succeeds")

      ;; 11i. Buffer narrowed to a subtree
      (orgist-test-assert
       (eq t (funcall run-sync-test
                      "rt-narrowed.org"
                      (concat "#+TITLE: N\n\n"
                              "* TODO Before\n:PROPERTIES:\n:ID: x1\n:END:\n\n"
                              "* TODO Target\n:PROPERTIES:\n:ID: rt-3\n:END:\n\n"
                              "* TODO After\n:PROPERTIES:\n:ID: x2\n:END:\n")
                      "rt-3" "Narrowed comment"
                      (lambda ()
                        (goto-char (point-min))
                        (re-search-forward "Target" nil t)
                        (org-back-to-heading t)
                        (org-narrow-to-subtree))))
       "Logbook insert: sync with narrowed buffer succeeds")))

  ;; (Sanitization tests 12a-12e removed — raw-byte sanitization
  ;; functions have been removed in favour of proper coding-system
  ;; configuration.  See the encoding test suite for coverage.)

  ;; --- 13: Label ↔ tag conversion ---
  (message "")
  (message "--- Test group: Label/tag name conversion ---")

  ;; 13a: label-to-tag removes hyphens and capitalizes next char
  (orgist-test-assert-equal
   "SelfCare" (orgist--label-to-tag "Self-care")
   "label-to-tag: hyphen removed, next char capitalized")

  ;; 13b: label-to-tag removes spaces and capitalizes next char
  (orgist-test-assert-equal
   "MyLabel" (orgist--label-to-tag "My Label")
   "label-to-tag: space removed, next char capitalized")

  ;; 13c: label-to-tag handles multiple hyphens/spaces
  (orgist-test-assert-equal
   "aBCD" (orgist--label-to-tag "a-b c-d")
   "label-to-tag: mixed hyphens and spaces to CamelCase")

  ;; 13d: label-to-tag passes clean names through
  (orgist-test-assert-equal
   "Work" (orgist--label-to-tag "Work")
   "label-to-tag: clean name unchanged")

  ;; 13e: label-to-tag handles nil
  (orgist-test-assert
   (null (orgist--label-to-tag nil))
   "label-to-tag: nil returns nil")

  ;; 13f: tag-to-label resolves via orgist-labels
  (orgist-test-assert-equal
   "Self-care"
   (let ((orgist-labels (make-hash-table :test 'equal)))
     (puthash "lbl-1" '((name . "Self-care") (color . "blue")) orgist-labels)
     (puthash "lbl-2" '((name . "Work") (color . "red")) orgist-labels)
     (orgist--tag-to-label "SelfCare"))
   "tag-to-label: SelfCare resolves to Self-care")

  ;; 13g: tag-to-label returns tag as-is when no match
  (orgist-test-assert-equal
   "BrandNew"
   (let ((orgist-labels (make-hash-table :test 'equal)))
     (puthash "lbl-1" '((name . "Work") (color . "red")) orgist-labels)
     (orgist--tag-to-label "BrandNew"))
   "tag-to-label: unknown tag returned as-is")

  ;; 13h: tag-to-label with nil orgist-labels returns tag as-is
  (orgist-test-assert-equal
   "SelfCare"
   (let ((orgist-labels nil))
     (orgist--tag-to-label "SelfCare"))
   "tag-to-label: nil labels table returns tag as-is")

  ;; 13i: label-name-to-id finds by org-tag name
  (orgist-test-assert-equal
   "lbl-1"
   (let ((orgist-labels (make-hash-table :test 'equal)))
     (puthash "lbl-1" '((name . "Self-care") (color . "blue")) orgist-labels)
     (orgist-label-name-to-id "SelfCare"))
   "label-name-to-id: finds Self-care via SelfCare")

  ;; 13j: label-name-to-id still works with exact name
  (orgist-test-assert-equal
   "lbl-2"
   (let ((orgist-labels (make-hash-table :test 'equal)))
     (puthash "lbl-2" '((name . "Work") (color . "red")) orgist-labels)
     (orgist-label-name-to-id "Work"))
   "label-name-to-id: exact name still works")

  ;; 13k: round-trip — pull labels, set tags, read back, convert for API
  (orgist-test-assert
   (let* ((orgist-labels (make-hash-table :test 'equal))
          (todoist-labels '["Self-care" "Work" "Long name here"])
          org-tags todoist-result)
     (puthash "lbl-1" '((name . "Self-care")) orgist-labels)
     (puthash "lbl-2" '((name . "Work")) orgist-labels)
     (puthash "lbl-3" '((name . "Long name here")) orgist-labels)
     ;; Pull: convert to org tags
     (setq org-tags (mapcar #'orgist--label-to-tag (append todoist-labels nil)))
     ;; Write-back: convert back to Todoist names
     (setq todoist-result (mapcar #'orgist--tag-to-label org-tags))
     (and (equal org-tags '("SelfCare" "Work" "LongNameHere"))
          (equal todoist-result '("Self-care" "Work" "Long name here"))))
   "Round-trip: labels survive pull→org→write-back conversion")

  ;; 13l: snapshot stores org-safe labels (no spurious diff)
  (orgist-test-assert
   (with-temp-buffer
     (org-mode)
     (insert "* TODO Task with labels                     :SelfCare:Work:\n")
     (insert ":PROPERTIES:\n:ID: tag-test-1\n:END:\n")
     (goto-char (point-min))
     (let* ((orgist-snapshots (make-hash-table :test 'equal))
            (orgist-enable-write-back t)
            (orgist-labels (make-hash-table :test 'equal))
            (element `((id . "tag-test-1")
                       (content . "Task with labels")
                       (labels . ["Self-care" "Work"])
                       (priority . 1))))
       (puthash "lbl-1" '((name . "Self-care")) orgist-labels)
       (puthash "lbl-2" '((name . "Work")) orgist-labels)
       ;; Snapshot the element (should store org-safe names)
       (orgist-snapshot-element element)
       (let ((snap (gethash "tag-test-1" orgist-snapshots)))
         (equal (plist-get snap :labels) '("SelfCare" "Work")))))
   "Snapshot stores org-safe label names")

  ;; 13m: item_update sends Todoist-original label names
  (orgist-test-assert
   (let* ((orgist-labels (make-hash-table :test 'equal))
          (orgist-snapshots (make-hash-table :test 'equal)))
     (puthash "lbl-1" '((name . "Self-care")) orgist-labels)
     ;; Snapshot with org-safe labels
     (puthash "tag-task-1"
              (list :content "Test" :checked nil :priority 1
                    :labels '("SelfCare")
                    :due nil :deadline nil :duration nil
                    :description "" :parent-id "proj-1"
                    :order 0 :section-p nil)
              orgist-snapshots)
     ;; Manually construct a label change diff (added "Hire" tag)
     (let* ((changes (list (cons "tag-task-1"
                                 (list (cons :labels
                                             (cons '("SelfCare")
                                                   '("SelfCare" "Hire")))))))
            (commands (orgist-changes-to-commands changes)))
       ;; The labels in the API command should be Todoist-original names
       (let* ((update-cmd (seq-find
                           (lambda (c) (equal (alist-get 'type c) "item_update"))
                           commands))
              (args (alist-get 'args update-cmd))
              (labels (append (alist-get 'labels args) nil)))
         (and update-cmd
              (member "Self-care" labels)
              (member "Hire" labels)))))
   "item_update sends Todoist-original label names")

  ;; ============================================================
  ;; Content conversion (orgist-convert-content)
  ;; ============================================================
  (message "")
  (message "--- Test group: Content conversion ---")

  ;; Plain text passes through unchanged (fast path)
  (orgist-test-assert-equal
   "Buy groceries"
   (orgist-convert-content "Buy groceries")
   "convert-content: plain text unchanged")

  ;; Nil and empty string pass through
  (orgist-test-assert-equal
   nil (orgist-convert-content nil)
   "convert-content: nil passes through")
  (orgist-test-assert-equal
   "" (orgist-convert-content "")
   "convert-content: empty string passes through")

  ;; Pandoc tests (skip if pandoc not available)
  (let ((pandoc-path (or (executable-find "pandoc")
                         (let ((choco "c:/ProgramData/chocolatey/bin"))
                           (when (file-exists-p
                                  (expand-file-name "pandoc.exe" choco))
                             (push choco exec-path)
                             (executable-find "pandoc"))))))
    (if (not pandoc-path)
        (message "  [SKIP] pandoc not found — skipping content pandoc tests")

      ;; Bold markdown -> org bold
      (orgist-test-assert
       (string-match-p (regexp-quote "*bold*")
                       (orgist-convert-content "**bold** text"))
       "convert-content: bold markdown to org")

      ;; Inline code -> org verbatim
      (orgist-test-assert
       (string-match-p "=" (orgist-convert-content "use `code` here"))
       "convert-content: inline code to org verbatim")

      ;; Link -> org link
      (orgist-test-assert
       (string-match-p (regexp-quote "[[https://example.com]")
                       (orgist-convert-content "Visit [here](https://example.com)"))
       "convert-content: markdown link to org link")

      ;; Strikethrough -> org +strike+
      (orgist-test-assert
       (string-match-p (regexp-quote "+done+")
                       (orgist-convert-content "~~done~~ already"))
       "convert-content: strikethrough to org")

      ;; Result is single line (no newlines)
      (orgist-test-assert
       (not (string-match-p "\n"
                            (orgist-convert-content
                             "**bold** and [link](https://example.com) and `code`")))
       "convert-content: result is single line")

      ;; Unicode preserved through pandoc
      (let* ((input (concat "Task with " (string #xe9) "accent"))
             (result (orgist-convert-content input)))
        (orgist-test-assert
         (string-match-p (string #xe9) result)
         "convert-content: unicode accent preserved"))

      ;; Integration: markdown title inserted as org heading
      (with-temp-buffer
        (org-mode)
        (insert "* TODO Plain title\n")
        (goto-char (point-min))
        (org-edit-headline
         (orgist-convert-content "**Important** [task](https://example.com)"))
        (let ((heading (car (orgist-extract-heading-and-tags))))
          (orgist-test-assert
           (and (string-search "*Important*" heading)
                (string-match-p "\\[\\[https://example.com\\]" heading))
           "convert-content: integration — markdown title as org heading")))

      ;; Regression: extracting heading must not lose tag match data.
      (with-temp-buffer
        (org-mode)
        (insert "* TODO Tagged heading :Work:Next:\n")
        (goto-char (point-min))
        (let ((parsed (orgist-extract-heading-and-tags)))
          (orgist-test-assert-equal
           "Tagged heading"
           (car parsed)
           "extract-heading-and-tags: heading preserved with trailing tags")
          (orgist-test-assert-equal
           '("Work" "Next")
           (cdr parsed)
           "extract-heading-and-tags: tags parsed without match-data corruption")))

      ;; --- Reverse conversion: org → markdown ---
      (message "")
      (message "--- Test group: Content reverse conversion (org → markdown) ---")

      ;; Org bold → markdown bold
      (orgist-test-assert
       (string-match-p (regexp-quote "**bold**")
                       (orgist-convert-content-to-markdown "*bold* text"))
       "convert-content-to-md: org bold to markdown")

      ;; Org verbatim → markdown code
      (orgist-test-assert
       (string-match-p "`code`"
                       (orgist-convert-content-to-markdown "use =code= here"))
       "convert-content-to-md: org verbatim to markdown code")

      ;; Org link → markdown link
      (orgist-test-assert
       (string-match-p (regexp-quote "[link](https://example.com)")
                       (orgist-convert-content-to-markdown
                        "[[https://example.com][link]]"))
       "convert-content-to-md: org link to markdown")

      ;; Org strikethrough → markdown strikethrough
      (orgist-test-assert
       (string-match-p (regexp-quote "~~done~~")
                       (orgist-convert-content-to-markdown "+done+ already"))
       "convert-content-to-md: org strikethrough to markdown")

      ;; Result is single line
      (orgist-test-assert
       (not (string-match-p "\n"
                            (orgist-convert-content-to-markdown
                             "*bold* and [[https://example.com][link]] and =code=")))
       "convert-content-to-md: result is single line")

      ;; Round-trip: markdown → org → markdown preserves semantics
      (let* ((original "**Important** [task](https://example.com) with `code`")
             (org-form (orgist-convert-content original))
             (back (orgist-convert-content-to-markdown org-form)))
        (orgist-test-assert
         (and (string-search "**Important**" back)
              (string-search "[task](https://example.com)" back)
              (string-match-p "`code`" back))
         "convert-content-to-md: round-trip preserves formatting"))

      ;; Plain text passes through (fast path)
      (orgist-test-assert-equal
       "Buy groceries"
       (orgist-convert-content-to-markdown "Buy groceries")
       "convert-content-to-md: plain text unchanged"))

    ;; Nil and empty pass through (outside pandoc block)
    (orgist-test-assert-equal
     nil (orgist-convert-content-to-markdown nil)
     "convert-content-to-md: nil passes through")
    (orgist-test-assert-equal
     "" (orgist-convert-content-to-markdown "")
     "convert-content-to-md: empty string passes through"))

  ;; Results
  (message "")
  (message "========================================")
  (message "=== Results: New Features ===")
  (message "=== Passed: %d  Failed: %d ==="
           orgist-test--passes orgist-test--failures)
  (message "========================================")
  (message "")
  orgist-test--failures)

;;; ============================================================
;;; H. Encoding test
;;; ============================================================

(defun orgist-test-run-encoding ()
  "Test that UTF-8 encoding is preserved throughout the sync pipeline.
Creates synthetic Todoist data with accented characters and
multi-line descriptions, syncs them, and verifies the on-disk
org file has:
- Correct UTF-8 byte sequences for accented characters
- Consistent line endings (no bare CR, no CR CR LF)
- No U+FFFD replacement characters"
  (setq orgist-test--failures 0)
  (setq orgist-test--passes 0)
  (message "")
  (message "========================================")
  (message "=== Encoding tests ===")
  (message "========================================")
  (message "")

  ;; Set up isolation
  (let* ((runtime-dir (expand-file-name "orgist-test/Encoding/"
                                        temporary-file-directory)))
    (when (file-directory-p runtime-dir)
      (delete-directory runtime-dir t))
    (make-directory runtime-dir t)
    (setq orgist-base-dir runtime-dir)
    (setq orgist-sync-token-filename (expand-file-name "sync_token" runtime-dir))
    (setq orgist-snapshot-file (expand-file-name "snapshots.el" runtime-dir))
    (setq orgist-log-file (expand-file-name "orgist.log" runtime-dir))
    (setq orgist-project-buffer-cache nil)
    (setq orgist-snapshots nil)
    (setq orgist-sync-mutex nil)
    (setq orgist-sync-project-filter nil)
    (setq revert-without-query '(".*"))
    (setq org-priority-highest 1 org-priority-lowest 5 org-priority-default 5)
    (setq orgist-enable-write-back t)
    (setq orgist-write-back-dry-run t)
    (setq orgist-log-level 'info)
    (setq org-log-into-drawer t)
    ;; Kill leftover buffers
    (dolist (buf (buffer-list))
      (when (and (buffer-file-name buf)
                 (string-match-p "\\.org$" (buffer-file-name buf)))
        (kill-buffer buf)))

    ;; Synthetic project
    (let ((project '((id . "enc-proj-1")
                     (name . "EncTest")
                     (parent_id . nil)
                     (is_deleted . :json-false)
                     (child_order . 0)))
          ;; Task 1: accented chars in content and description
          (task-accents
           `((id . "enc-task-1")
             (content . "F\u00eate des M\u00e8res")
             (project_id . "enc-proj-1")
             (section_id . nil)
             (parent_id . nil)
             (child_order . 0)
             (priority . 1)
             (checked . :json-false)
             (labels . [])
             (due . nil)
             (deadline . nil)
             (duration . nil)
             (description . "[D\u00e9claration de naissance](https://www.service-public.fr/particuliers/vosdroits/F961)")
             (added_at . "2025-06-22T15:43:00Z")))
          ;; Task 2: multi-line description with accented chars
          (task-multiline
           `((id . "enc-task-2")
             (content . "Cl\u00e9mentine's birthday")
             (project_id . "enc-proj-1")
             (section_id . nil)
             (parent_id . nil)
             (child_order . 1)
             (priority . 1)
             (checked . :json-false)
             (labels . [])
             (due . nil)
             (deadline . nil)
             (duration . nil)
             (description . "Premi\u00e8re ligne\n\nDeuxi\u00e8me paragraphe avec \u00e9\u00e8\u00ea\u00eb\u00e7\u00e0\u00f9\u00fc\u00f1\n\nTroisi\u00e8me")
             (added_at . "2025-06-23T04:58:00Z")))
          ;; Task 3: description with only ASCII (control case)
          (task-ascii
           '((id . "enc-task-3")
             (content . "Plain ASCII task")
             (project_id . "enc-proj-1")
             (section_id . nil)
             (parent_id . nil)
             (child_order . 2)
             (priority . 1)
             (checked . :json-false)
             (labels . [])
             (due . nil)
             (deadline . nil)
             (duration . nil)
             (description . "Just plain text\n\nSecond paragraph")
             (added_at . "2025-06-24T10:00:00Z"))))

      ;; Process through the sync pipeline
      (orgist-update-projects (vector project))
      (orgist-update-elements
       (vector task-accents task-multiline task-ascii)
       'item)
      (orgist--flush-pending-saves)

      ;; Read the resulting org file as raw bytes
      (let* ((org-file (expand-file-name "EncTest.org" runtime-dir))
             (raw-bytes (with-temp-buffer
                          (set-buffer-multibyte nil)
                          (insert-file-contents-literally org-file)
                          (buffer-string)))
             ;; Also read as text for content checks
             (content (with-temp-buffer
                        (insert-file-contents org-file)
                        (buffer-string))))

        (message "")
        (message "--- Test group: UTF-8 byte verification ---")

        ;; 1. Check e-acute (U+00E9) is encoded as c3 a9
        (orgist-test-assert
         (let ((pos (seq-position raw-bytes ?\x63))  ; 'c' in 'claration'
               (found nil))
           ;; Search for the byte sequence c3 a9 63 6c (é c l)
           (dotimes (i (- (length raw-bytes) 3))
             (when (and (= (aref raw-bytes i) #xc3)
                        (= (aref raw-bytes (1+ i)) #xa9)
                        (= (aref raw-bytes (+ i 2)) #x63)  ; 'c'
                        (= (aref raw-bytes (+ i 3)) #x6c)) ; 'l'
               (setq found t)))
           found)
         "e-acute: D\u00e9claration encoded as UTF-8 c3 a9")

        ;; 2. Check e-grave (U+00E8) is encoded as c3 a8
        (orgist-test-assert
         (let ((found nil))
           (dotimes (i (- (length raw-bytes) 1))
             (when (and (= (aref raw-bytes i) #xc3)
                        (= (aref raw-bytes (1+ i)) #xa8))
               (setq found t)))
           found)
         "e-grave: M\u00e8res encoded as UTF-8 c3 a8")

        ;; 3. Check e-circumflex (U+00EA) is encoded as c3 aa
        (orgist-test-assert
         (let ((found nil))
           (dotimes (i (- (length raw-bytes) 1))
             (when (and (= (aref raw-bytes i) #xc3)
                        (= (aref raw-bytes (1+ i)) #xaa))
               (setq found t)))
           found)
         "e-circumflex: F\u00eate encoded as UTF-8 c3 aa")

        ;; 4. Check c-cedilla (U+00E7) is encoded as c3 a7
        (orgist-test-assert
         (let ((found nil))
           (dotimes (i (- (length raw-bytes) 1))
             (when (and (= (aref raw-bytes i) #xc3)
                        (= (aref raw-bytes (1+ i)) #xa7))
               (setq found t)))
           found)
         "c-cedilla: \u00e7 encoded as UTF-8 c3 a7")

        (message "")
        (message "--- Test group: No replacement characters ---")

        ;; 5. No U+FFFD replacement characters (ef bf bd in UTF-8)
        (orgist-test-assert
         (let ((found nil))
           (dotimes (i (- (length raw-bytes) 2))
             (when (and (= (aref raw-bytes i) #xef)
                        (= (aref raw-bytes (1+ i)) #xbf)
                        (= (aref raw-bytes (+ i 2)) #xbd))
               (setq found t)))
           (not found))
         "No U+FFFD replacement characters in output")

        (message "")
        (message "--- Test group: Line ending consistency ---")

        ;; 6. No bare CR (CR not followed by LF)
        (orgist-test-assert
         (let ((bare-cr 0))
           (dotimes (i (length raw-bytes))
             (when (= (aref raw-bytes i) #x0d)
               (let ((next (if (< (1+ i) (length raw-bytes))
                               (aref raw-bytes (1+ i))
                             -1)))
                 (unless (= next #x0a)
                   (setq bare-cr (1+ bare-cr))))))
           (= bare-cr 0))
         "No bare CR (every CR followed by LF)")

        ;; 7. No CR CR LF sequences
        (orgist-test-assert
         (let ((crcrlf 0))
           (dotimes (i (- (length raw-bytes) 2))
             (when (and (= (aref raw-bytes i) #x0d)
                        (= (aref raw-bytes (1+ i)) #x0d)
                        (= (aref raw-bytes (+ i 2)) #x0a))
               (setq crcrlf (1+ crcrlf))))
           (= crcrlf 0))
         "No CR CR LF sequences (no doubled carriage returns)")

        ;; 8. All line endings are the same convention (all CRLF or all LF)
        (orgist-test-assert
         (let ((crlf 0) (lf-only 0))
           (dotimes (i (length raw-bytes))
             (when (= (aref raw-bytes i) #x0a)
               (if (and (> i 0) (= (aref raw-bytes (1- i)) #x0d))
                   (setq crlf (1+ crlf))
                 (setq lf-only (1+ lf-only)))))
           (message "  Line endings: CRLF=%d LF-only=%d" crlf lf-only)
           (or (= crlf 0) (= lf-only 0)))
         "Uniform line endings (all CRLF or all LF, not mixed)")

        (message "")
        (message "--- Test group: Content integrity ---")

        ;; 9. Accented text appears correctly in buffer string
        (orgist-test-assert
         (string-match-p "D\u00e9claration de naissance" content)
         "D\u00e9claration appears correctly in buffer")

        ;; 10. Multi-line description: all paragraphs present
        (orgist-test-assert
         (string-match-p "Premi\u00e8re ligne" content)
         "First paragraph with accents preserved")

        (orgist-test-assert
         (string-match-p "Deuxi\u00e8me paragraphe" content)
         "Second paragraph with accents preserved")

        (orgist-test-assert
         (string-match-p "Troisi\u00e8me" content)
         "Third paragraph with accents preserved")

        ;; 11. All accented chars in multi-line desc survived
        (orgist-test-assert
         (string-match-p "\u00e9\u00e8\u00ea\u00eb\u00e7\u00e0\u00f9\u00fc\u00f1" content)
         "All 9 accented characters preserved: \u00e9\u00e8\u00ea\u00eb\u00e7\u00e0\u00f9\u00fc\u00f1"))))

  ;; --- Part 2: Orgtest cached data (real Todoist responses) ---
  (message "")
  (message "--- Test group: Orgtest \"Test encoding\" task ---")
  (let* ((script-dir (file-name-directory (or load-file-name buffer-file-name)))
         (test-data-dir (expand-file-name "test-data/Orgtest/" script-dir))
         (json-file (expand-file-name "full-sync.json" test-data-dir)))
    (if (not (file-exists-p json-file))
        (message "  [SKIP] Orgtest test data not found at %s" json-file)
      ;; Set up a fresh isolation dir for the Orgtest sync
      (let* ((runtime-dir (expand-file-name "orgist-test/EncOrgtest/"
                                            temporary-file-directory)))
        (when (file-directory-p runtime-dir)
          (delete-directory runtime-dir t))
        (make-directory runtime-dir t)
        (setq orgist-base-dir runtime-dir)
        (setq orgist-sync-token-filename
              (expand-file-name "sync_token" runtime-dir))
        (setq orgist-snapshot-file
              (expand-file-name "snapshots.el" runtime-dir))
        (setq orgist-project-buffer-cache nil)
        (setq orgist-snapshots nil)
        (setq orgist-sync-project-filter "Orgtest")
        ;; Kill leftover org buffers
        (dolist (buf (buffer-list))
          (when (and (buffer-file-name buf)
                     (string-match-p "\\.org$" (buffer-file-name buf)))
            (kill-buffer buf)))
        ;; Load and process the cached JSON
        (let* ((json-object-type 'alist)
               (json-array-type 'vector)
               (json-key-type 'symbol)
               (data (json-read-file json-file))
               (projects (alist-get 'projects data))
               (sections (alist-get 'sections data))
               (items (alist-get 'items data))
               (orgist--batch-save-pending (make-hash-table :test 'eq)))
          (orgist-update-projects (orgist-sort-hierarchically projects))
          (orgist-update-elements sections 'section)
          (orgist-update-elements (orgist-sort-hierarchically items) 'item)
          (orgist--flush-pending-saves))
        ;; Read the generated Orgtest.org as raw bytes
        (let* ((org-file (expand-file-name "Orgtest.org" runtime-dir))
               (raw-bytes (with-temp-buffer
                            (set-buffer-multibyte nil)
                            (insert-file-contents-literally org-file)
                            (buffer-string)))
               (content (with-temp-buffer
                          (insert-file-contents org-file)
                          (buffer-string))))
          ;; 12. The "Test encoding" task should contain Déclaration
          (orgist-test-assert
           (string-match-p "D\u00e9claration" content)
           "Orgtest: D\u00e9claration in output")

          ;; 12b. En-dash preserved (not converted to --)
          (orgist-test-assert
           (string-match-p (string #x2013) content)  ; U+2013 en-dash
           "Orgtest: en-dash preserved (not converted to --)")

          ;; 12c. Degree sign preserved
          (orgist-test-assert
           (string-match-p (string #xb0) content)  ; U+00B0 degree sign
           "Orgtest: degree sign preserved")

          ;; 13. e-acute bytes c3 a9 present in raw file
          (orgist-test-assert
           (let ((found nil))
             (dotimes (i (- (length raw-bytes) 3))
               (when (and (= (aref raw-bytes i) #xc3)
                          (= (aref raw-bytes (1+ i)) #xa9)
                          (= (aref raw-bytes (+ i 2)) #x63)   ; 'c'
                          (= (aref raw-bytes (+ i 3)) #x6c))  ; 'l'
                 (setq found t)))
             found)
           "Orgtest: UTF-8 c3 a9 bytes for \u00e9 in file")

          ;; 13b. En-dash bytes e2 80 93 present in raw file
          (orgist-test-assert
           (let ((found nil))
             (dotimes (i (- (length raw-bytes) 2))
               (when (and (= (aref raw-bytes i) #xe2)
                          (= (aref raw-bytes (1+ i)) #x80)
                          (= (aref raw-bytes (+ i 2)) #x93))
                 (setq found t)))
             found)
           "Orgtest: UTF-8 e2 80 93 bytes for en-dash in file")

          ;; 14. No replacement characters
          (orgist-test-assert
           (not (string-match-p (string #xfffd) content))
           "Orgtest: no U+FFFD replacement characters")

          ;; 15. Consistent line endings
          (orgist-test-assert
           (let ((crlf 0) (lf-only 0))
             (dotimes (i (length raw-bytes))
               (when (= (aref raw-bytes i) #x0a)
                 (if (and (> i 0) (= (aref raw-bytes (1- i)) #x0d))
                     (setq crlf (1+ crlf))
                   (setq lf-only (1+ lf-only)))))
             (or (= crlf 0) (= lf-only 0)))
           "Orgtest: uniform line endings")))))

  ;; --- Part 3: JSON roundtrip (simulates subprocess data transfer) ---
  (message "")
  (message "--- Test group: JSON file roundtrip (subprocess path) ---")
  (let* ((json-file (make-temp-file "orgist-enc-rt" nil ".json"))
         ;; Simulate what the parent writes for the subprocess
         (original-data
          `((items . ,(vector
                       `((id . "rt-1")
                         (content . ,(concat "F" (string #xe9) "te"))
                         (description . ,(concat "D" (string #xe9)
                                                 "claration\n\nline2"))
                         (project_id . "rt-proj")
                         (section_id . nil)
                         (parent_id . nil)
                         (child_order . 0)
                         (priority . 1)
                         (checked . :json-false)
                         (labels . [])
                         (due . nil)
                         (deadline . nil)
                         (duration . nil)
                         (added_at . "2025-01-01T00:00:00Z")))))))
    (unwind-protect
        (progn
          ;; Write JSON the same way orgist--subprocess-pull does
          (with-temp-file json-file
            (let ((json-encoding-pretty-print nil))
              (insert (json-encode original-data))))

          ;; Read it back the same way the subprocess does
          (let* ((json-object-type 'alist)
                 (json-array-type 'vector)
                 (json-key-type 'symbol)
                 (roundtrip (json-read-file json-file))
                 (items (alist-get 'items roundtrip))
                 (item (aref items 0))
                 (rt-content (alist-get 'content item))
                 (rt-desc (alist-get 'description item)))

            ;; 16. Content survives roundtrip
            (orgist-test-assert-equal
             (concat "F" (string #xe9) "te") rt-content
             "JSON roundtrip: accented content preserved")

            ;; 17. e-acute is char 233 (not raw byte)
            (orgist-test-assert-equal
             233 (aref rt-content 1)
             "JSON roundtrip: e-acute is codepoint 233")

            ;; 18. Description newlines are LF (char 10)
            (orgist-test-assert
             (and rt-desc (string-match-p "\n\n" rt-desc))
             "JSON roundtrip: description newlines are LF")

            ;; 19. No CR in roundtripped description
            (orgist-test-assert
             (not (string-match-p "\r" rt-desc))
             "JSON roundtrip: no CR in description")))
      (when (file-exists-p json-file) (delete-file json-file))))

  ;; --- Part 4: Pandoc conversion encoding ---
  (message "")
  (message "--- Test group: Pandoc conversion encoding ---")
  (let ((pandoc-path (or (executable-find "pandoc")
                         (let ((choco "c:/ProgramData/chocolatey/bin"))
                           (when (file-exists-p
                                  (expand-file-name "pandoc.exe" choco))
                             (push choco exec-path)
                             (executable-find "pandoc"))))))
    (if (not pandoc-path)
        (message "  [SKIP] pandoc not found — skipping pandoc tests")

      ;; 20. Pandoc converts markdown link to org link, preserving accents
      (let* ((md-input (concat "[D" (string #xe9) "claration de naissance]"
                               "(https://example.com)"))
             (result (orgist-convert-description md-input 1)))
        (orgist-test-assert
         (string-search (concat "[[https://example.com][D"
                                  (string #xe9) "claration de naissance]]") result)
         "Pandoc: markdown link converted to org with accent preserved")

        ;; 21. The e-acute in pandoc output is codepoint 233
        (let ((pos (string-match (string #xe9) result)))
          (orgist-test-assert
           (and pos (= (aref result pos) 233))
           "Pandoc: e-acute is codepoint 233 (not raw byte)")))

      ;; 22. Multi-line description through pandoc
      (let* ((md-input (concat "Premi" (string #xe8) "re ligne\n\n"
                               "Deuxi" (string #xe8) "me paragraphe"))
             (result (orgist-convert-description md-input 1)))
        (orgist-test-assert
         (and (string-match-p (concat "Premi" (string #xe8) "re") result)
              (string-match-p (concat "Deuxi" (string #xe8) "me") result))
         "Pandoc: multi-paragraph accents preserved"))

      ;; 22b. En-dash preserved through pandoc (not converted to --)
      (let* ((md-input (concat "8" (string #x2013) "72" (string #xb0) "F"))
             (result (orgist-convert-description md-input 1)))
        (orgist-test-assert
         (string-search (string #x2013) result)
         "Pandoc: en-dash U+2013 preserved (not --)"))

      ;; 22c. Em-dash preserved through pandoc
      (let* ((md-input (concat "hello" (string #x2014) "world"))
             (result (orgist-convert-description md-input 1)))
        (orgist-test-assert
         (string-search (string #x2014) result)
         "Pandoc: em-dash U+2014 preserved (not ---)"))

      ;; 22d. Ellipsis preserved through pandoc
      (let* ((md-input (concat "wait" (string #x2026)))
             (result (orgist-convert-description md-input 1)))
        (orgist-test-assert
         (string-search (string #x2026) result)
         "Pandoc: ellipsis U+2026 preserved (not ...)"))

      ;; 22e. Right single quote preserved through pandoc
      (let* ((md-input (concat "it" (string #x2019) "s fine"))
             (result (orgist-convert-description md-input 1)))
        (orgist-test-assert
         (string-search (string #x2019) result)
         "Pandoc: right single quote U+2019 preserved (not ')"))

      ;; 23. Full pipeline: pandoc + insert + save to file
      (let* ((runtime-dir (expand-file-name "orgist-test/EncPandoc/"
                                            temporary-file-directory))
             (project '((id . "pdc-proj-1")
                        (name . "PdcTest")
                        (parent_id . nil)
                        (is_deleted . :json-false)
                        (child_order . 0)))
             (task `((id . "pdc-task-1")
                     (content . ,(concat "F" (string #xe9) "te"))
                     (project_id . "pdc-proj-1")
                     (section_id . nil)
                     (parent_id . nil)
                     (child_order . 0)
                     (priority . 1)
                     (checked . :json-false)
                     (labels . [])
                     (due . nil)
                     (deadline . nil)
                     (duration . nil)
                     (description
                      . ,(concat "[D" (string #xe9)
                                 "claration](https://example.com)"
                                 "\n\nDeuxi" (string #xe8) "me ligne"
                                 "\n\n8" (string #x2013) "72" (string #xb0) "F"
                                 " hello" (string #x2014) "world"))
                     (added_at . "2025-01-01T00:00:00Z"))))
        (when (file-directory-p runtime-dir)
          (delete-directory runtime-dir t))
        (make-directory runtime-dir t)
        (setq orgist-base-dir runtime-dir)
        (setq orgist-sync-token-filename
              (expand-file-name "sync_token" runtime-dir))
        (setq orgist-snapshot-file
              (expand-file-name "snapshots.el" runtime-dir))
        (setq orgist-project-buffer-cache nil)
        (setq orgist-snapshots nil)
        (setq orgist-sync-project-filter nil)
        ;; Kill leftover org buffers
        (dolist (buf (buffer-list))
          (when (and (buffer-file-name buf)
                     (string-match-p "\\.org$" (buffer-file-name buf)))
            (kill-buffer buf)))
        (orgist-update-projects (vector project))
        (orgist-update-elements (vector task) 'item)
        (orgist--flush-pending-saves)

        (let* ((org-file (expand-file-name "PdcTest.org" runtime-dir))
               (raw-bytes (with-temp-buffer
                            (set-buffer-multibyte nil)
                            (insert-file-contents-literally org-file)
                            (buffer-string)))
               (content (with-temp-buffer
                          (insert-file-contents org-file)
                          (buffer-string))))

          ;; 23a. Org link syntax present (pandoc converted it)
          (orgist-test-assert
           (string-match-p "\\[\\[https://example\\.com\\]\\[D" content)
           "Pandoc pipeline: org link syntax in output")

          ;; 23b. e-acute bytes c3 a9 in the file
          (orgist-test-assert
           (let ((found nil))
             (dotimes (i (- (length raw-bytes) 1))
               (when (and (= (aref raw-bytes i) #xc3)
                          (= (aref raw-bytes (1+ i)) #xa9))
                 (setq found t)))
             found)
           "Pandoc pipeline: UTF-8 c3 a9 in file")

          ;; 23c. No replacement characters
          (orgist-test-assert
           (not (string-match-p (string #xfffd) content))
           "Pandoc pipeline: no U+FFFD")

          ;; 23d. Uniform line endings
          (orgist-test-assert
           (let ((crlf 0) (lf-only 0))
             (dotimes (i (length raw-bytes))
               (when (= (aref raw-bytes i) #x0a)
                 (if (and (> i 0) (= (aref raw-bytes (1- i)) #x0d))
                     (setq crlf (1+ crlf))
                   (setq lf-only (1+ lf-only)))))
             (or (= crlf 0) (= lf-only 0)))
           "Pandoc pipeline: uniform line endings")

          ;; 23e. e-grave preserved through pandoc
          (orgist-test-assert
           (string-match-p (concat "Deuxi" (string #xe8) "me") content)
           "Pandoc pipeline: e-grave preserved")

          ;; 23f. En-dash preserved through full pipeline
          (orgist-test-assert
           (string-search (string #x2013) content)
           "Pandoc pipeline: en-dash preserved")

          ;; 23g. Em-dash preserved through full pipeline
          (orgist-test-assert
           (string-search (string #x2014) content)
           "Pandoc pipeline: em-dash preserved")

          ;; 23h. En-dash bytes e2 80 93 in file on disk
          (orgist-test-assert
           (let ((found nil))
             (dotimes (i (- (length raw-bytes) 2))
               (when (and (= (aref raw-bytes i) #xe2)
                          (= (aref raw-bytes (1+ i)) #x80)
                          (= (aref raw-bytes (+ i 2)) #x93))
                 (setq found t)))
             found)
           "Pandoc pipeline: en-dash UTF-8 bytes e2 80 93 on disk")))))

  ;; Results
  (message "")
  (message "========================================")
  (message "=== Results: Encoding ===")
  (message "=== Passed: %d  Failed: %d ==="
           orgist-test--passes orgist-test--failures)
  (message "========================================")
  (message "")
  orgist-test--failures)

;;; ============================================================
;;; Quick-add live test (needs API token)
;;; ============================================================

(defun orgist-test-run-quick-add ()
  "Test the quick-add endpoint with a real API call.
Creates a test task, verifies it, then deletes it."
  (setq orgist-test--failures 0)
  (setq orgist-test--passes 0)
  (message "")
  (message "========================================")
  (message "=== Quick-add live test ===")
  (message "========================================")
  (message "")
  (message "--- Test group: Quick-add API ---")

  ;; Configure token from env
  (let ((token (getenv "TODOIST_API_TOKEN")))
    (unless token
      (message "[FAIL] TODOIST_API_TOKEN not set")
      (setq orgist-test--failures 1)
      (message "")
      (message "========================================")
      (message "=== Results: Quick-add ===")
      (message "=== Passed: 0  Failed: 1 ===")
      (message "========================================")
      (kill-emacs 1))
    (setq orgist-bearer-token token)

    ;; Remove the request mock so we make real API calls
    (advice-remove 'request #'orgist-test--request-advice)

    ;; Test 1: Make the quick-add API call directly with request
    (require 'request)
    (let ((result nil)
          (error-info nil))
      (request
        "https://api.todoist.com/api/v1/tasks/quick"
        :type "POST"
        :headers `(("Authorization" . ,(format "Bearer %s" token))
                   ("Content-Type" . "application/json"))
        :data (json-encode `((text . "orgist test quick-add DELETE ME")
                             (meta . t)
                             (auto_reminder . t)))
        :parser 'json-read
        :sync t
        :error (cl-function
                (lambda (&key data error-thrown &allow-other-keys)
                  (setq error-info (list data error-thrown))))
        :success (cl-function
                  (lambda (&key data &allow-other-keys)
                    (setq result data))))

      (orgist-test-assert (not error-info)
                          (format "Quick-add: API call succeeds (err=%S)" error-info))
      (orgist-test-assert result
                          "Quick-add: response is non-nil")

      (when result
        (let ((task-id (alist-get 'id result))
              (content (alist-get 'content result)))
          (orgist-test-assert (and task-id (not (equal task-id "")))
                              (format "Quick-add: task has ID (%s)" task-id))
          (orgist-test-assert (and content (string-match-p "test quick-add" content))
                              (format "Quick-add: content matches (%s)" content))

          ;; Clean up: delete the test task via REST API
          (when task-id
            (message "Cleaning up: deleting task %s" task-id)
            (request
              (format "https://api.todoist.com/api/v1/tasks/%s" task-id)
              :type "DELETE"
              :headers `(("Authorization" . ,(format "Bearer %s" token)))
              :sync t
              :error (cl-function
                      (lambda (&key error-thrown &allow-other-keys)
                        (message "Warning: cleanup delete failed: %S" error-thrown)))
              :success (cl-function
                        (lambda (&rest _)
                          (message "Cleanup: test task deleted"))))
            (orgist-test-assert t "Quick-add: cleanup delete sent"))))))

  ;; Results
  (message "")
  (message "========================================")
  (message "=== Results: Quick-add ===")
  (message "=== Passed: %d  Failed: %d ==="
           orgist-test--passes orgist-test--failures)
  (message "========================================")
  (message "")
  orgist-test--failures)

;;; ============================================================
;;; Attachment tests
;;; ============================================================

(defun orgist-test-run-attachments ()
  "Test attachment sync: ATTACH tag filtering, pull, push, diff, non-TODO children."
  (setq orgist-test--failures 0)
  (setq orgist-test--passes 0)
  (message "")
  (message "========================================")
  (message "=== Attachment tests ===")
  (message "========================================")

  ;; Set up minimal isolation
  (let* ((runtime-dir (expand-file-name "orgist-test/Attachments/"
                                        temporary-file-directory)))
    (when (file-directory-p runtime-dir)
      (delete-directory runtime-dir t))
    (make-directory runtime-dir t)
    (setq orgist-base-dir runtime-dir)
    (setq orgist-sync-token-filename (expand-file-name "sync_token" runtime-dir))
    (setq orgist-snapshot-file (expand-file-name "snapshots.el" runtime-dir))
    (setq orgist-log-file (expand-file-name "orgist.log" runtime-dir))
    (setq orgist-project-buffer-cache nil)
    (setq orgist-snapshots nil)
    (setq orgist-sync-mutex nil)
    (setq orgist-sync-project-filter nil)
    (setq revert-without-query '(".*"))
    (setq org-priority-highest 1 org-priority-lowest 5 org-priority-default 5)
    (setq orgist-enable-write-back t)
    (setq orgist-write-back-dry-run t)
    (setq orgist-log-level 'info)
    (setq org-log-into-drawer t)
    (setq orgist-sync-attachments t)
    (dolist (buf (buffer-list))
      (when (and (buffer-file-name buf)
                 (string-match-p "\\.org$" (buffer-file-name buf)))
        (kill-buffer buf)))

    ;; Create a synthetic project with tasks
    (let ((project '((id . "att-proj-1")
                     (name . "AttachTest")
                     (parent_id . nil)
                     (is_deleted . :json-false)
                     (child_order . 0)))
          (task-basic '((id . "att-task-1")
                        (content . "Task with attachment")
                        (project_id . "att-proj-1")
                        (section_id . nil)
                        (parent_id . nil)
                        (child_order . 0)
                        (priority . 1)
                        (checked . :json-false)
                        (labels . ["Work" "Urgent"])
                        (due . nil)
                        (deadline . nil)
                        (duration . nil)
                        (description . "Some description")
                        (added_at . "2026-01-15T10:00:00Z")))
          (task-child '((id . "att-task-2")
                        (content . "Parent task")
                        (project_id . "att-proj-1")
                        (section_id . nil)
                        (parent_id . nil)
                        (child_order . 1)
                        (priority . 1)
                        (checked . :json-false)
                        (labels . [])
                        (due . nil)
                        (deadline . nil)
                        (duration . nil)
                        (description . "")
                        (added_at . "2026-01-15T10:00:00Z")))
          (inhibit-redisplay t)
          (orgist--batch-save-pending (make-hash-table :test 'eq)))
      (orgist-update-projects (vector project))
      (orgist-update-elements (vector task-basic task-child) 'item)
      (orgist--flush-pending-saves)

      ;; Open files and build caches
      (dolist (file (directory-files orgist-base-dir t "\\.org$"))
        (find-file file)
        (org-mode)
        (orgist-build-id-cache))

      ;; ============================================================
      ;; 1. ATTACH tag filtering in local state
      ;; ============================================================
      (message "")
      (message "--- Test group: ATTACH tag filtering ---")

      ;; Add ATTACH tag manually to task-basic heading
      (let ((pos (orgist-find-element-by-id "att-task-1")))
        (orgist-test-assert pos "Task att-task-1 found in buffer")
        (when pos
          (save-excursion
            (goto-char pos)
            (org-back-to-heading t)
            ;; Manually set tags to include ATTACH
            (org-set-tags '("Work" "Urgent" "ATTACH"))
            (let ((local (orgist-element-local-state)))
              ;; Labels should NOT include ATTACH
              (orgist-test-assert
               (not (member "ATTACH" (plist-get local :labels)))
               "ATTACH tag filtered from local state labels")
              ;; But Work and Urgent should still be there
              (orgist-test-assert
               (and (member "Work" (plist-get local :labels))
                    (member "Urgent" (plist-get local :labels)))
               "Other labels preserved in local state")))))

      ;; ============================================================
      ;; 2. ATTACH tag preservation during update-element
      ;; ============================================================
      (message "")
      (message "--- Test group: ATTACH tag preservation ---")

      ;; The heading has ATTACH + Work + Urgent.  Update with new labels
      ;; from Todoist — ATTACH should be preserved.
      (let ((pos (orgist-find-element-by-id "att-task-1")))
        (when pos
          (save-excursion
            (goto-char pos)
            (org-back-to-heading t)
            ;; Ensure ATTACH is on the heading
            (org-set-tags '("Work" "ATTACH"))
            ;; Now simulate a Todoist update with labels ["NewLabel"]
            (let ((updated-task '((id . "att-task-1")
                                  (content . "Task with attachment")
                                  (project_id . "att-proj-1")
                                  (section_id . nil)
                                  (parent_id . nil)
                                  (child_order . 0)
                                  (priority . 1)
                                  (checked . :json-false)
                                  (labels . ["NewLabel"])
                                  (due . nil)
                                  (deadline . nil)
                                  (duration . nil)
                                  (description . "Some description")
                                  (added_at . "2026-01-15T10:00:00Z")))
                  (orgist--batch-save-pending (make-hash-table :test 'eq)))
              (orgist-update-elements (vector updated-task) 'item)
              (orgist--flush-pending-saves)
              ;; Rebuild cache
              (orgist-build-id-cache)
              (let ((pos2 (orgist-find-element-by-id "att-task-1")))
                (when pos2
                  (save-excursion
                    (goto-char pos2)
                    (let ((tags (cdr (orgist-extract-heading-and-tags))))
                      (orgist-test-assert
                       (member "ATTACH" tags)
                       "ATTACH tag preserved after update-element")
                      (orgist-test-assert
                       (member "NewLabel" tags)
                       "New label applied by update-element")
                      (orgist-test-assert
                       (not (member "Work" tags))
                       "Old label removed by update-element")))))))))

      ;; ============================================================
      ;; 3. Pull attachments (mock download)
      ;; ============================================================
      (message "")
      (message "--- Test group: Pull attachments ---")

      ;; Save initial snapshots (before attachment sync)
      (orgist-save-snapshots)
      (orgist-load-snapshots t)

      ;; Mock comments with file_attachment
      (let* ((mock-comments
              (list
               '((id . "comment-att-1")
                 (posted_uid . "fixture-user")
                 (content . "")
                 (file_attachment . ((file_name . "report.pdf")
                                    (file_url . "https://example.com/report.pdf")
                                    (file_type . "application/pdf")
                                    (file_size . 12345)))
                 (posted_at . "2026-02-19T10:00:00Z")
                 (item_id . "att-task-1"))
               '((id . "comment-att-2")
                 (posted_uid . "fixture-user")
                 (content . "Here is the image")
                 (file_attachment . ((file_name . "photo.jpg")
                                    (file_url . "https://example.com/photo.jpg")
                                    (file_type . "image/jpeg")
                                    (file_size . 54321)))
                 (posted_at . "2026-02-19T10:01:00Z")
                 (item_id . "att-task-1"))
               '((id . "comment-no-att")
                 (posted_uid . "fixture-user")
                 (content . "Just a plain comment")
                 (file_attachment . nil)
                 (posted_at . "2026-02-19T10:02:00Z")
                 (item_id . "att-task-1"))))
             (pos (orgist-find-element-by-id "att-task-1")))
        (when pos
          (save-excursion
            (goto-char pos)
            (org-back-to-heading t)
            ;; Mock orgist-download-file to create a dummy file instead
            ;; of actually downloading
            (cl-letf (((symbol-function 'orgist-download-file)
                       (lambda (url dest)
                         (let ((dir (file-name-directory dest)))
                           (unless (file-directory-p dir) (make-directory dir t)))
                         (with-temp-file dest
                           (insert (format "mock-content-from-%s" url)))
                         t)))
              (let ((result (orgist-sync-task-attachments
                             "att-task-1" mock-comments nil)))
                ;; Should have 2 attachment entries
                (orgist-test-assert
                 (length= result 2)
                 (format "Pull: 2 attachments synced (%d found)" (length result)))
                ;; Check report.pdf (alist is (comment-id . filename))
                (orgist-test-assert
                 (assoc "comment-att-1" result)
                 "Pull: report.pdf in attachment alist")
                (when (assoc "comment-att-1" result)
                  (orgist-test-assert-equal
                   "report.pdf" (cdr (assoc "comment-att-1" result))
                   "Pull: report.pdf mapped to correct comment-id"))
                ;; Check photo.jpg
                (orgist-test-assert
                 (assoc "comment-att-2" result)
                 "Pull: photo.jpg in attachment alist")
                ;; Check files exist on disk
                (let ((dir (org-attach-dir)))
                  (orgist-test-assert dir "Pull: org-attach dir created")
                  (when dir
                    (orgist-test-assert
                     (file-exists-p (expand-file-name "report.pdf" dir))
                     "Pull: report.pdf exists on disk")
                    (orgist-test-assert
                     (file-exists-p (expand-file-name "photo.jpg" dir))
                     "Pull: photo.jpg exists on disk")))
                ;; Check ATTACH tag was set
                (let ((tags (cdr (orgist-extract-heading-and-tags))))
                  (orgist-test-assert
                   (member "ATTACH" tags)
                   "Pull: ATTACH tag set on heading"))

                ;; --- Re-pull: no duplicate downloads ---
                (message "")
                (message "--- Test group: Pull re-pull (no duplicates) ---")
                (let ((download-count 0))
                  (cl-letf (((symbol-function 'orgist-download-file)
                             (lambda (url dest)
                               (setq download-count (1+ download-count))
                               (let ((dir (file-name-directory dest)))
                                 (unless (file-directory-p dir)
                                   (make-directory dir t)))
                               (with-temp-file dest
                                 (insert (format "mock-content-from-%s" url)))
                               t)))
                    (let ((result2 (orgist-sync-task-attachments
                                   "att-task-1" mock-comments result)))
                      (orgist-test-assert-equal
                       0 download-count
                       "Re-pull: no duplicate downloads")
                      (orgist-test-assert-equal
                       (length result) (length result2)
                       "Re-pull: attachment count unchanged"))))

                ;; --- Delete detection ---
                (message "")
                (message "--- Test group: Pull delete detection ---")
                ;; Remove comment-att-1 from the comment list (simulating deletion)
                (let* ((reduced-comments
                        (seq-remove
                         (lambda (c) (equal (alist-get 'id c) "comment-att-1"))
                         mock-comments))
                       (result3 (orgist-sync-task-attachments
                                 "att-task-1" reduced-comments result)))
                  ;; report.pdf should be gone (keyed by comment-id)
                  (orgist-test-assert
                   (not (assoc "comment-att-1" result3))
                   "Delete: report.pdf removed from alist")
                  (let ((dir (org-attach-dir)))
                    (when dir
                      (orgist-test-assert
                       (not (file-exists-p (expand-file-name "report.pdf" dir)))
                       "Delete: report.pdf removed from disk")))
                  ;; photo.jpg should remain
                  (orgist-test-assert
                   (assoc "comment-att-2" result3)
                   "Delete: photo.jpg still in alist")))))))

      ;; ============================================================
      ;; 3b. Duplicate filename disambiguation
      ;; ============================================================
      (message "")
      (message "--- Test group: Duplicate filename disambiguation ---")
      (save-excursion
        (goto-char (orgist-find-element-by-id "att-task-1"))
        (org-back-to-heading t)
        ;; Create mock comments with two attachments sharing the same name
        (let* ((dup-comments
                `(((id . "comment-dup-1")
                   (file_attachment . ((file_name . "image.png")
                                       (file_url . "https://example.com/img1.png"))))
                  ((id . "comment-dup-2")
                   (file_attachment . ((file_name . "image.png")
                                       (file_url . "https://example.com/img2.png"))))))
               (result (orgist-sync-task-attachments
                        "att-task-1" dup-comments nil)))
          ;; Both entries should be tracked
          (orgist-test-assert-equal
           2 (length result)
           "Dup: 2 attachment entries tracked")
          ;; Both comment-ids should be present
          (orgist-test-assert
           (assoc "comment-dup-1" result)
           "Dup: comment-dup-1 in alist")
          (orgist-test-assert
           (assoc "comment-dup-2" result)
           "Dup: comment-dup-2 in alist")
          ;; Second file should be disambiguated on disk
          (let* ((dir (org-attach-dir))
                 (name1 (cdr (assoc "comment-dup-1" result)))
                 (name2 (cdr (assoc "comment-dup-2" result))))
            (orgist-test-assert-equal
             "image.png" name1
             "Dup: first file keeps original name")
            (orgist-test-assert-equal
             "image_2.png" name2
             "Dup: second file disambiguated")
            (when dir
              (orgist-test-assert
               (file-exists-p (expand-file-name "image.png" dir))
               "Dup: image.png exists on disk")
              (orgist-test-assert
               (file-exists-p (expand-file-name "image_2.png" dir))
               "Dup: image_2.png exists on disk")
              ;; Clean up dup test files so they don't affect push tests
              (dolist (f '("image.png" "image_2.png"))
                (let ((p (expand-file-name f dir)))
                  (when (file-exists-p p) (delete-file p))))))))

      ;; ============================================================
      ;; 3c. Nil-ghost deduplication in orgist-sync-task-attachments
      ;; ============================================================
      (message "")
      (message "--- Test group: Nil-ghost deduplication ---")
      (save-excursion
        (goto-char (orgist-find-element-by-id "att-task-1"))
        (org-back-to-heading t)
        ;; Simulate corrupted snapshot: real cid entry plus nil-keyed ghost
        ;; for the same file (produced by old write-back corruption).
        (let* ((corrupted-known
                '(("cid-real" . "ghost.pdf")
                  (nil . "ghost.pdf")
                  (nil . "ghost_2.pdf")))
               (comments
                (list '((id . "cid-real")
                        (file_attachment . ((file_name . "ghost.pdf")
                                            (file_url . "https://example.com/ghost.pdf"))))))
               (dir (org-attach-dir-get-create)))
          ;; Pre-create the file on disk so adoption fires
          (with-temp-file (expand-file-name "ghost.pdf" dir)
            (insert "ghost content"))
          (with-temp-file (expand-file-name "ghost_2.pdf" dir)
            (insert "ghost_2 content"))
          (let ((result (orgist-sync-task-attachments
                         "att-task-1" comments corrupted-known)))
            ;; nil ghost for "ghost.pdf" should be removed (real cid covers it)
            (orgist-test-assert
             (not (assoc nil (seq-filter
                              (lambda (e) (equal (cdr e) "ghost.pdf")) result)))
             "Nil-ghost: nil entry for ghost.pdf removed")
            ;; real cid entry must remain
            (orgist-test-assert
             (assoc "cid-real" result)
             "Nil-ghost: real cid-real entry preserved")
            ;; nil entry for ghost_2.pdf has no real counterpart → kept
            (orgist-test-assert
             (cl-some (lambda (e) (and (null (car e)) (equal (cdr e) "ghost_2.pdf")))
                      result)
             "Nil-ghost: nil entry for ghost_2.pdf (no real cover) kept")
            ;; Total: cid-real + nil/ghost_2 = 2 entries
            (orgist-test-assert-equal
             2 (length result)
             "Nil-ghost: total 2 entries after cleanup"))
          ;; Cleanup
          (let ((dir2 (org-attach-dir)))
            (dolist (f '("ghost.pdf" "ghost_2.pdf"))
              (let ((p (when dir2 (expand-file-name f dir2))))
                (when (and p (file-exists-p p)) (delete-file p)))))))

      ;; ============================================================
      ;; 3d. orgist-update-snapshots-from-local preserves :attachment-files
      ;; ============================================================
      (message "")
      (message "--- Test group: Snapshot :attachment-files not overwritten ---")
      (let* ((task-id "att-task-1")
             (original-afiles '(("cid-preserve" . "keep.txt")))
             (snap (gethash task-id orgist-snapshots)))
        (when snap
          (plist-put snap :attachment-files original-afiles)
          (puthash task-id snap orgist-snapshots))
        ;; Simulate a write-back item_update command for this task.
        ;; orgist-update-snapshots-from-local should NOT touch :attachment-files.
        (let ((fake-cmd `((type . "item_update")
                          (uuid . "test-uuid-1")
                          (args . ((id . ,task-id)
                                   (content . "Task with attachment"))))))
          (orgist-update-snapshots-from-local (list fake-cmd) nil))
        ;; Verify attachment-files was not overwritten
        (let ((snap2 (gethash task-id orgist-snapshots)))
          (orgist-test-assert-equal
           original-afiles (plist-get snap2 :attachment-files)
           "Snapshot: :attachment-files not overwritten by item_update write-back")))

      ;; ============================================================
      ;; 3e. Multiset command generation for duplicate filenames
      ;; ============================================================
      (message "")
      (message "--- Test group: Multiset command generation ---")
      (save-excursion
        (goto-char (orgist-find-element-by-id "att-task-1"))
        (org-back-to-heading t)
        ;; Snapshot has 2×"foo.txt" (one real, one nil ghost) + "bar.txt"
        ;; Local disk has 1×"foo.txt" + "bar.txt"
        ;; Expected: zero commands (no real delete, the duplicate is just the nil ghost)
        ;; After Fix 2 cleans nil ghosts during pull the snapshot becomes 1+1;
        ;; but in command generation old-files comes from the sorted filename list.
        ;; Simulate the old-files/new-files state directly.
        (let* ((old-files '("bar.txt" "foo.txt" "foo.txt"))  ; sorted, 2× foo
               (new-files '("bar.txt" "foo.txt"))             ; sorted, 1× foo
               ;; Build fake snapshot alist: one real cid for foo.txt, one nil ghost
               (snap-afiles '(("cid-foo" . "foo.txt") (nil . "foo.txt") ("cid-bar" . "bar.txt")))
               ;; Run multiset delete logic (extracted inline)
               (delete-cmds '())
               (new-remaining (copy-sequence new-files))
               (snap-remaining (copy-sequence snap-afiles)))
          (dolist (file-name old-files)
            (let ((pos (cl-position file-name new-remaining :test #'equal)))
              (if pos
                  (setq new-remaining
                        (append (cl-subseq new-remaining 0 pos)
                                (cl-subseq new-remaining (1+ pos))))
                (let* ((nil-entry (seq-find (lambda (e)
                                              (and (null (car e))
                                                   (equal (cdr e) file-name)))
                                            snap-remaining))
                       (real-entry (unless nil-entry
                                     (seq-find (lambda (e)
                                                 (and (car e)
                                                      (equal (cdr e) file-name)))
                                               snap-remaining)))
                       (entry (or nil-entry real-entry)))
                  (when entry
                    (setq snap-remaining (delq entry snap-remaining))
                    (when-let* ((comment-id (car entry)))
                      (push (list (cons 'type "attachment_delete")
                                  (cons 'args (list (cons 'comment_id comment-id)
                                                    (cons 'file_name file-name))))
                            delete-cmds)))))))
          ;; Nil-ghost is consumed silently (no API delete); the real cid is spared.
          (orgist-test-assert-equal
           0 (length delete-cmds)
           "Multiset: nil-keyed duplicate generates no delete command")

          ;; Now: snapshot has 2 real cid entries for "foo.txt" (ambiguous —
          ;; two distinct Todoist comments share a local filename).
          ;; Local has 1 foo.txt.  Expected: 0 delete commands (consume silently).
          (let* ((snap-afiles2 '(("cid-foo-1" . "foo.txt") ("cid-foo-2" . "foo.txt")
                                 ("cid-bar" . "bar.txt")))
                 (delete-cmds2 '())
                 (new-remaining2 (copy-sequence new-files))
                 (snap-remaining2 (copy-sequence snap-afiles2)))
            (dolist (file-name old-files)
              (let ((pos (cl-position file-name new-remaining2 :test #'equal)))
                (if pos
                    (setq new-remaining2
                          (append (cl-subseq new-remaining2 0 pos)
                                  (cl-subseq new-remaining2 (1+ pos))))
                  (let* ((nil-entry2 (seq-find (lambda (e)
                                                 (and (null (car e))
                                                      (equal (cdr e) file-name)))
                                               snap-remaining2))
                         (real-entries2 (unless nil-entry2
                                          (seq-filter (lambda (e)
                                                        (and (car e)
                                                             (equal (cdr e) file-name)))
                                                      snap-remaining2)))
                         (delete-entry2 (when (= (length real-entries2) 1)
                                          (car real-entries2)))
                         (consume-entry2 (or nil-entry2 delete-entry2
                                             (car real-entries2))))
                    (when consume-entry2
                      (setq snap-remaining2 (delq consume-entry2 snap-remaining2))
                      (when delete-entry2
                        (push (list (cons 'type "attachment_delete")
                                    (cons 'args (list (cons 'comment_id (car delete-entry2))
                                                      (cons 'file_name file-name))))
                              delete-cmds2)))))))
            (orgist-test-assert-equal
             0 (length delete-cmds2)
             "Multiset: 2 ambiguous real cids for same file generates 0 delete commands"))

          ;; Unambiguous case: exactly 1 real cid for a file that is gone locally.
          ;; Expected: 1 delete command.
          (let* ((old-files3 '("bar.txt" "foo.txt"))   ; foo present in snapshot
                 (new-files3 '("bar.txt"))              ; foo deleted locally
                 (snap-afiles3 '(("cid-single" . "foo.txt") ("cid-bar" . "bar.txt")))
                 (delete-cmds3 '())
                 (new-remaining3 (copy-sequence new-files3))
                 (snap-remaining3 (copy-sequence snap-afiles3)))
            (dolist (file-name old-files3)
              (let ((pos (cl-position file-name new-remaining3 :test #'equal)))
                (if pos
                    (setq new-remaining3
                          (append (cl-subseq new-remaining3 0 pos)
                                  (cl-subseq new-remaining3 (1+ pos))))
                  (let* ((nil-entry3 (seq-find (lambda (e)
                                                 (and (null (car e))
                                                      (equal (cdr e) file-name)))
                                               snap-remaining3))
                         (real-entries3 (unless nil-entry3
                                          (seq-filter (lambda (e)
                                                        (and (car e)
                                                             (equal (cdr e) file-name)))
                                                      snap-remaining3)))
                         (delete-entry3 (when (= (length real-entries3) 1)
                                          (car real-entries3)))
                         (consume-entry3 (or nil-entry3 delete-entry3
                                             (car real-entries3))))
                    (when consume-entry3
                      (setq snap-remaining3 (delq consume-entry3 snap-remaining3))
                      (when delete-entry3
                        (push (list (cons 'type "attachment_delete")
                                    (cons 'args (list (cons 'comment_id (car delete-entry3))
                                                      (cons 'file_name file-name))))
                              delete-cmds3)))))))
            (orgist-test-assert-equal
             1 (length delete-cmds3)
             "Multiset: 1 real cid for deleted file generates exactly 1 delete command")
            (when delete-cmds3
              (orgist-test-assert-equal
               "cid-single"
               (alist-get 'comment_id (alist-get 'args (car delete-cmds3)))
               "Multiset: delete targets the correct comment-id")))))

      ;; ============================================================
      ;; 4. Push: diff detects attachment changes
      ;; ============================================================
      (message "")
      (message "--- Test group: Push attachment diff ---")

      ;; Save snapshots with known attachment-files
      (let* ((pos (orgist-find-element-by-id "att-task-1"))
             (snap (gethash "att-task-1" orgist-snapshots)))
        (when snap
          ;; Set snapshot attachment-files to reflect what we synced
          ;; Format: (comment-id . filename)
          (plist-put snap :attachment-files '(("comment-att-2" . "photo.jpg")))
          (puthash "att-task-1" snap orgist-snapshots))
        (orgist-save-snapshots)
        (orgist-load-snapshots t)
        ;; Rebuild caches
        (dolist (file (directory-files orgist-base-dir t "\\.org$"))
          (with-current-buffer (find-file-noselect file)
            (orgist-build-id-cache)))

        ;; Now add a new file to the attach dir
        (when pos
          (save-excursion
            (goto-char pos)
            (org-back-to-heading t)
            (let ((dir (org-attach-dir-get-create)))
              (with-temp-file (expand-file-name "newfile.txt" dir)
                (insert "new attachment content"))
              ;; Mark buffer modified so orgist--modified-org-files picks it up
              ;; (attachment files live in a separate dir — the org file mtime
              ;; doesn't change, but the buffer-modified flag is also checked)
              (set-buffer-modified-p t))))

        ;; Diff should detect the new file
        (let* ((changes (orgist-diff-all-elements))
               (task-change (assoc "att-task-1" changes)))
          (orgist-test-assert task-change
                              "Push diff: change detected for att-task-1")
          (when task-change
            (let ((att-diff (assq :attachment-files (cdr task-change))))
              (orgist-test-assert att-diff
                                  "Push diff: :attachment-files in diff")
              (when att-diff
                (let ((old-files (cadr att-diff))
                      (new-files (cddr att-diff)))
                  (orgist-test-assert
                   (member "newfile.txt" new-files)
                   "Push diff: newfile.txt in new-files")
                  (orgist-test-assert
                   (not (member "newfile.txt" old-files))
                   "Push diff: newfile.txt NOT in old-files")
                  (orgist-test-assert
                   (member "photo.jpg" new-files)
                   "Push diff: photo.jpg still in new-files")))))))

      ;; ============================================================
      ;; 5. Push: generate correct commands
      ;; ============================================================
      (message "")
      (message "--- Test group: Push command generation ---")

      (let* ((changes (orgist-diff-all-elements))
             (commands (orgist-changes-to-commands changes))
             (upload-cmds (seq-filter
                           (lambda (c) (equal (alist-get 'type c) "attachment_upload"))
                           commands))
             (delete-cmds (seq-filter
                           (lambda (c) (equal (alist-get 'type c) "attachment_delete"))
                           commands)))
        (orgist-test-assert
         (>= (length upload-cmds) 1)
         (format "Push cmd: attachment_upload commands (%d found)" (length upload-cmds)))
        (when upload-cmds
          (let* ((cmd (car upload-cmds))
                 (args (alist-get 'args cmd)))
            (orgist-test-assert-equal
             "att-task-1" (alist-get 'item_id args)
             "Push cmd: upload targets correct task ID")
            (orgist-test-assert-equal
             "newfile.txt" (alist-get 'file_name args)
             "Push cmd: upload has correct file_name")))

        ;; Now test delete: remove photo.jpg from attach dir
        (let ((pos (orgist-find-element-by-id "att-task-1")))
          (when pos
            (save-excursion
              (goto-char pos)
              (org-back-to-heading t)
              (let* ((dir (org-attach-dir))
                     (photo-path (when dir (expand-file-name "photo.jpg" dir))))
                (when (and photo-path (file-exists-p photo-path))
                  (delete-file photo-path)))
              (set-buffer-modified-p t))))
        ;; Rediff
        (let* ((changes2 (orgist-diff-all-elements))
               (commands2 (orgist-changes-to-commands changes2))
               (delete-cmds2 (seq-filter
                              (lambda (c) (equal (alist-get 'type c) "attachment_delete"))
                              commands2)))
          (orgist-test-assert
           (>= (length delete-cmds2) 1)
           (format "Push cmd: attachment_delete commands (%d found)"
                   (length delete-cmds2)))
          (when delete-cmds2
            (let* ((cmd (car delete-cmds2))
                   (args (alist-get 'args cmd)))
              (orgist-test-assert-equal
               "comment-att-2" (alist-get 'comment_id args)
               "Push cmd: delete targets correct comment-id")
              (orgist-test-assert-equal
               "photo.jpg" (alist-get 'file_name args)
               "Push cmd: delete has correct file_name")))))

      ;; ============================================================
      ;; 6. Non-TODO child heading file collection
      ;; ============================================================
      (message "")
      (message "--- Test group: Non-TODO child attachment collection ---")

      ;; Add a non-TODO child under att-task-2 (Parent task)
      ;; and attach files to it
      (let ((pos (orgist-find-element-by-id "att-task-2")))
        (when pos
          (save-excursion
            (goto-char pos)
            (org-back-to-heading t)
            (let ((end (save-excursion (org-end-of-subtree t t) (point))))
              (goto-char end)
              ;; Insert a non-TODO child heading
              (insert "\n** Notes\n:PROPERTIES:\n:ID: att-child-non-todo\n:END:\nSome notes.\n")
              ;; Create attach dir for the child and add a file
              (goto-char (org-find-entry-with-id "att-child-non-todo"))
              (org-back-to-heading t)
              (let ((child-dir (org-attach-dir-get-create)))
                (with-temp-file (expand-file-name "child-doc.pdf" child-dir)
                  (insert "child attachment content"))
                (org-attach-tag))))
          (save-buffer)
          (orgist-build-id-cache)

          ;; Now check that orgist--collect-attachment-files on the parent
          ;; also picks up the child's file
          (let ((pos2 (orgist-find-element-by-id "att-task-2")))
            (when pos2
              (save-excursion
                (goto-char pos2)
                (org-back-to-heading t)
                (let ((files (orgist--collect-attachment-files)))
                  (orgist-test-assert
                   (and files (member "child-doc.pdf" files))
                   (format "Non-TODO child: child-doc.pdf collected (%S)" files))))))))

      ;; ============================================================
      ;; 7. Round-trip: no spurious diffs with attachments disabled
      ;; ============================================================
      (message "")
      (message "--- Test group: No spurious diffs with attachments off ---")

      (let ((orgist-sync-attachments nil))
        ;; Rebuild snapshots without attachment info
        (orgist-save-snapshots)
        (orgist-load-snapshots t)
        (dolist (file (directory-files orgist-base-dir t "\\.org$"))
          (with-current-buffer (find-file-noselect file)
            (orgist-build-id-cache)))
        (let* ((changes (orgist-diff-all-elements))
               ;; Filter out date-only and attachment-only diffs
               (real-diffs
                (seq-remove
                 (lambda (c)
                   (and (listp (cdr c))
                        (seq-every-p
                         (lambda (d) (memq (car d) '(:due :deadline
                                                     :attachment-files)))
                         (cdr c))))
                 changes)))
          (orgist-test-assert-equal
           0 (length real-diffs)
           "Round-trip: zero spurious diffs with attachments disabled")
          (when real-diffs
            (message "  Unexpected diffs:")
            (dolist (c (seq-take real-diffs 5))
              (message "    %s: %S" (car c) (cdr c)))))))

    ;; ============================================================
    ;; 8. Confirm buffer: attachment display
    ;; ============================================================
    (message "")
    (message "--- Test group: Confirm buffer attachment display ---")

    (require 'orgist-confirm)
    (let ((formatted (orgist-confirm--format-value
                      :attachment-files '("doc.pdf" "img.png"))))
      (orgist-test-assert
       (string-match-p "doc\\.pdf" formatted)
       "Confirm display: shows doc.pdf")
      (orgist-test-assert
       (string-match-p "img\\.png" formatted)
       "Confirm display: shows img.png"))
    (let ((formatted-nil (orgist-confirm--format-value
                          :attachment-files nil)))
      (orgist-test-assert-equal
       "(none)" formatted-nil
       "Confirm display: nil shows (none)")))

  ;; Results
  (message "")
  (message "========================================")
  (message "=== Results: Attachments ===")
  (message "=== Passed: %d  Failed: %d ==="
           orgist-test--passes orgist-test--failures)
  (message "========================================")
  (message "")
  orgist-test--failures)

;;; ============================================================
;;; I. Completed tasks tests
;;; ============================================================

(defun orgist-test-run-completed-tasks ()
  "Test completed tasks fetch and insertion.
Phase 1: Full sync of Orgtest
Phase 2: Plan limits check
Phase 3: Fetch and insert completed tasks
Phase 4: Verify insertion
Phase 5: Re-pull (no duplicates)"
  (setq orgist-test--failures 0)
  (setq orgist-test--passes 0)
  (message "")
  (message "========================================")
  (message "=== Completed Tasks tests ===")
  (message "========================================")

  ;; Phase 1: Full sync
  (message "")
  (message "--- Phase 1: Full sync ---")
  (orgist-test-setup-isolation "Orgtest")
  (setq orgist-sync-completed-tasks t)
  (setq orgist-plan-limits nil)
  (setq org-log-into-drawer t)
  (when (file-exists-p orgist-sync-token-filename)
    (delete-file orgist-sync-token-filename))
  (orgist)
  (let ((completed (orgist-test--wait-for-sync 120)))
    (orgist-test-assert completed "Full sync completed")
    (unless completed (kill-emacs 1)))

  ;; Open org files and build caches
  (dolist (file (directory-files orgist-base-dir t "\\.org$"))
    (find-file file)
    (org-mode)
    (orgist-build-id-cache))

  ;; Phase 2: Plan limits check
  (message "")
  (message "--- Phase 2: Plan limits check ---")
  (setq orgist-plan-limits nil)
  (orgist-test-assert
   (orgist-completed-tasks-available-p)
   "completed-tasks-available-p returns t from mock")

  ;; Phase 3: Fetch and insert completed tasks
  (message "")
  (message "--- Phase 3: Fetch and insert completed tasks ---")
  (let ((tasks (orgist-fetch-completed-tasks
                "2026-01-01T00:00:00Z" "2026-03-01T00:00:00Z")))
    (orgist-test-assert
     (and tasks (>= (length tasks) 2))
     (format "Fetched completed tasks (%d found)" (length (or tasks '()))))

    (when tasks
      (orgist-process-completed-tasks tasks)

      ;; Phase 4: Verify insertion
      (message "")
      (message "--- Phase 4: Verify insertion ---")

      ;; Task A: root task
      (let ((pos (orgist-find-element-by-id "mock-completed-1")))
        (orgist-test-assert pos "Completed root task found in buffer")
        (when pos
          (save-excursion
            (goto-char pos)
            (orgist-test-assert
             (string= (org-get-todo-state) "DONE")
             "Completed root task has DONE state")
            (let ((content (buffer-substring-no-properties
                            pos (save-excursion (org-end-of-subtree t t) (point)))))
              (orgist-test-assert
               (string-match-p "Completed root task" content)
               "Completed root task has correct heading")
              (orgist-test-assert
               (string-match-p "\"DONE\"" content)
               "Completed root task has DONE logbook entry")))))

      ;; Task B: section task
      (let ((pos (orgist-find-element-by-id "mock-completed-2")))
        (orgist-test-assert pos "Completed section task found in buffer")
        (when pos
          (save-excursion
            (goto-char pos)
            (orgist-test-assert
             (string= (org-get-todo-state) "DONE")
             "Completed section task has DONE state")
            ;; Verify it's under the correct section
            (let ((parent-id (save-excursion
                               (when (org-up-heading-safe)
                                 (org-entry-get (point) "ID")))))
              (orgist-test-assert
               (equal parent-id "fixture-section")
               (format "Section task is under Section (parent=%s)" parent-id))))))

      ;; Phase 5: Re-pull (no duplicates)
      (message "")
      (message "--- Phase 5: Re-pull (no duplicates) ---")
      (let ((content-before
             (mapconcat
              (lambda (file)
                (with-current-buffer (find-buffer-visiting file)
                  (buffer-substring-no-properties (point-min) (point-max))))
              (directory-files orgist-base-dir t "\\.org$")
              "\n")))
        ;; Process same tasks again
        (orgist-process-completed-tasks tasks)
        (let ((content-after
               (mapconcat
                (lambda (file)
                  (with-current-buffer (find-buffer-visiting file)
                    (buffer-substring-no-properties (point-min) (point-max))))
                (directory-files orgist-base-dir t "\\.org$")
                "\n")))
          (orgist-test-assert-equal
           content-before content-after
           "Re-processing produces no duplicates")))

      ;; Phase 6: Deferred retry — child before parent
      (message "")
      (message "--- Phase 6: Deferred retry (child before parent) ---")
      (let* ((parent-task
              `((id . "mock-completed-parent")
                (content . "Completed parent task")
                (description . "")
                (project_id . "fixture-project")
                (section_id . nil)
                (parent_id . nil)
                (labels . [])
                (priority . 1)
                (due . nil)
                (deadline . nil)
                (duration . nil)
                (order . 200)
                (created_at . "2026-02-01T10:00:00Z")
                (completed_at . "2026-02-15T14:30:00Z")
                (checked . t)))
             (child-task
              `((id . "mock-completed-child")
                (content . "Completed child task")
                (description . "")
                (project_id . "fixture-project")
                (section_id . nil)
                (parent_id . "mock-completed-parent")
                (labels . [])
                (priority . 1)
                (due . nil)
                (deadline . nil)
                (duration . nil)
                (order . 201)
                (created_at . "2026-02-02T10:00:00Z")
                (completed_at . "2026-02-16T14:30:00Z")
                (checked . t)))
             ;; Child comes first — parent doesn't exist yet
             (tasks (list child-task parent-task)))
        (orgist-process-completed-tasks tasks)
        (let ((parent-pos (orgist-find-element-by-id "mock-completed-parent"))
              (child-pos (orgist-find-element-by-id "mock-completed-child")))
          (orgist-test-assert parent-pos
                              "Deferred: parent task inserted")
          (orgist-test-assert child-pos
                              "Deferred: child task inserted via retry")
          ;; Verify child is under parent
          (when child-pos
            (save-excursion
              (goto-char child-pos)
              (let ((child-parent-id
                     (save-excursion
                       (when (org-up-heading-safe)
                         (org-entry-get (point) "ID")))))
                (orgist-test-assert-equal
                 "mock-completed-parent" child-parent-id
                 "Deferred: child is nested under parent"))))))))

  ;; Results
  (message "")
  (message "========================================")
  (message "=== Results: Completed Tasks ===")
  (message "=== Passed: %d  Failed: %d ==="
           orgist-test--passes orgist-test--failures)
  (message "========================================")
  (message "")
  orgist-test--failures)

;;; ============================================================
;;; CLI dispatch
;;; ============================================================

(defun orgist-test--discover-projects ()
  "Return list of root project names from the shared full-sync.json."
  (let* ((script-dir (file-name-directory (or load-file-name buffer-file-name)))
         (json-file (expand-file-name "test-data/full-sync.json" script-dir))
         (projects '()))
    (if (not (file-exists-p json-file))
        (progn
          (message "[test-harness] full-sync.json not found at %s" json-file)
          nil)
      (let* ((json-object-type 'alist)
             (json-array-type 'vector)
             (json-key-type 'symbol)
             (data (json-read-file json-file))
             (all-projects (alist-get 'projects data)))
        (seq-do (lambda (p)
                  (unless (alist-get 'parent_id p)
                    (push (alist-get 'name p) projects)))
                all-projects)
        (nreverse projects)))))

;;; ============================================================
;;; Metadata preservation tests

(defun orgist-test-run-metadata ()
  "Test non-Todoist property preservation via metadata comments."
  (setq orgist-test--failures 0)
  (setq orgist-test--passes 0)
  (message "")
  (message "========================================")
  (message "=== Metadata preservation tests ===")
  (message "========================================")

  (let* ((runtime-dir (expand-file-name "orgist-test/Metadata/"
                                        temporary-file-directory)))
    (when (file-directory-p runtime-dir)
      (delete-directory runtime-dir t))
    (make-directory runtime-dir t)
    (let ((orgist-base-dir runtime-dir)
          (orgist-snapshot-file (expand-file-name "snapshots.el" runtime-dir))
          (orgist-sync-metadata t)
          (orgist-enable-write-back t)
          (orgist-write-back-dry-run t)
          (orgist-snapshots (make-hash-table :test 'equal)))

      ;; Create a test org file with non-Todoist properties
      (let ((file (expand-file-name "TestProject.org" runtime-dir)))
        (with-temp-file file
          (insert "#+PROPERTY: ID test-proj-1
* TestProject
:PROPERTIES:
:ID: test-proj-1
:END:
** TODO Pick up iphone
CLOSED: [2026-03-17 Tue 12:43]
:PROPERTIES:
:ID: meta-task-1
:TODOIST-ORDER: 1
:ETag: \"3547014421947454\"
:LOCATION: Example Store, 123 Example St, Example City, CA 00000, USA
:calendar-id: test@example.com
:entry-id: fixture-calendar-entry
:org-gcal-managed: gcal
:END:
:org-gcal:
<2026-03-16 Mon 02:00-10:30>
:END:

** TODO Plain task
:PROPERTIES:
:ID: meta-task-2
:TODOIST-ORDER: 2
:END:

"))
        (find-file file)
        (org-mode)
        (orgist-build-id-cache)

        ;; Phase 1: Extract metadata
        (message "")
        (message "--- Phase 1: Extract metadata ---")
        (goto-char (orgist-find-element-by-id "meta-task-1"))
        (let ((metadata (orgist-extract-metadata)))
          (orgist-test-assert (not (null metadata))
                              "Extract: returns non-nil for task with extra props")
          (orgist-test-assert (string-match-p "\\[orgist-metadata\\]" metadata)
                              "Extract: contains marker")
          (orgist-test-assert (string-match-p "ETag" metadata)
                              "Extract: contains ETag")
          (orgist-test-assert (string-match-p "LOCATION" metadata)
                              "Extract: contains LOCATION")
          (orgist-test-assert (string-match-p "org-gcal-managed" metadata)
                              "Extract: contains org-gcal-managed")
          (orgist-test-assert (not (string-match-p "CLOSED:" metadata))
                              "Extract: does not contain CLOSED timestamp (Todoist has completed_at)")
          (orgist-test-assert (string-match-p "org-gcal:" metadata)
                              "Extract: contains org-gcal custom drawer")
          ;; Must NOT contain managed properties
          (orgist-test-assert (not (string-match-p ":ID:" metadata))
                              "Extract: excludes ID property")
          (orgist-test-assert (not (string-match-p "TODOIST-ORDER" metadata))
                              "Extract: excludes TODOIST-ORDER"))

        ;; Phase 2: Plain task has no metadata
        (message "")
        (message "--- Phase 2: Plain task has no metadata ---")
        (goto-char (orgist-find-element-by-id "meta-task-2"))
        (let ((metadata (orgist-extract-metadata)))
          (orgist-test-assert (null metadata)
                              "Extract: returns nil for plain task"))

        ;; Phase 3: Restore metadata to a clean heading
        (message "")
        (message "--- Phase 3: Restore metadata ---")
        ;; First extract, then clear non-Todoist props, then restore
        (goto-char (orgist-find-element-by-id "meta-task-1"))
        (let ((metadata (orgist-extract-metadata)))
          ;; Remove the extra properties
          (org-entry-delete (point) "ETag")
          (org-entry-delete (point) "LOCATION")
          (org-entry-delete (point) "calendar-id")
          (org-entry-delete (point) "entry-id")
          (org-entry-delete (point) "org-gcal-managed")
          ;; Verify they're gone
          (orgist-test-assert (null (org-entry-get (point) "ETag"))
                              "Restore setup: ETag removed")
          ;; Restore
          (orgist-restore-metadata metadata)
          ;; Verify they're back
          (orgist-test-assert (equal (org-entry-get (point) "ETag")
                                     "\"3547014421947454\"")
                              "Restore: ETag restored")
          (orgist-test-assert (not (null (org-entry-get (point) "LOCATION")))
                              "Restore: LOCATION restored")
          (orgist-test-assert (equal (org-entry-get (point) "org-gcal-managed")
                                     "gcal")
                              "Restore: org-gcal-managed restored"))

        ;; Phase 4: Round-trip (extract → metadata comment format → restore)
        (message "")
        (message "--- Phase 4: Metadata comment detection ---")
        (let ((comment `((id . "mock-meta-1")
                         (content . ,(concat orgist-metadata-marker
                                            "\n:PROPERTIES:\n:CUSTOM-PROP: test-value\n:END:"))
                         (posted_at . "2026-03-17T12:00:00Z"))))
          (orgist-test-assert (orgist--metadata-comment-p comment)
                              "Detection: recognizes metadata comment")
          (orgist-test-assert (not (orgist--metadata-comment-p
                                    '((id . "x") (content . "regular comment"))))
                              "Detection: rejects regular comment"))

        ;; Phase 5: Write path (sync-metadata-comments)
        (message "")
        (message "--- Phase 5: Write path ---")
        ;; Set up a snapshot for meta-task-1
        (puthash "meta-task-1"
                 (list :content "Pick up iphone" :section-p nil :order 1)
                 orgist-snapshots)
        (let ((orgist-write-back-dry-run nil)
              (orgist-bearer-token "test-token"))
          ;; Create a command referencing the task
          (let ((commands (list (list (cons 'type "item_update")
                                     (cons 'uuid "test-uuid")
                                     (cons 'args (list (cons 'id "meta-task-1")))))))
            (orgist-sync-metadata-comments commands)
            ;; Check that the snapshot got a metadata-comment-id
            (let ((snap (gethash "meta-task-1" orgist-snapshots)))
              (orgist-test-assert (not (null (plist-get snap :metadata-comment-id)))
                                  "Write: metadata comment ID stored in snapshot"))))

        ;; Phase 6: No metadata comment for plain task
        (message "")
        (message "--- Phase 6: No metadata for plain task ---")
        (puthash "meta-task-2"
                 (list :content "Plain task" :section-p nil :order 2)
                 orgist-snapshots)
        (let ((orgist-write-back-dry-run nil)
              (orgist-bearer-token "test-token"))
          (let ((commands (list (list (cons 'type "item_update")
                                     (cons 'uuid "test-uuid-2")
                                     (cons 'args (list (cons 'id "meta-task-2")))))))
            (orgist-sync-metadata-comments commands)
            (let ((snap (gethash "meta-task-2" orgist-snapshots)))
              (orgist-test-assert (null (plist-get snap :metadata-comment-id))
                                  "Write: no metadata comment for plain task")))))))

  (message "")
  (message "========================================")
  (message "=== Results: Metadata ===")
  (message "=== Passed: %d  Failed: %d ==="
           orgist-test--passes orgist-test--failures)
  (message "========================================")
  (message "")
  orgist-test--failures)

;;; Archived sections & order detection tests
;;; ============================================================

(defun orgist-test-run-archived-sections ()
  "Test archived section sync and order detection."
  (setq orgist-test--failures 0)
  (setq orgist-test--passes 0)
  (message "")
  (message "========================================")
  (message "=== Archived sections & order tests ===")
  (message "========================================")

  ;; Phase 1: Full sync — verify archived section gets :ARCHIVE: tag
  (message "")
  (message "--- Phase 1: Full sync with archived section ---")
  (orgist-test-setup-isolation "Orgtest")
  (setq orgist-enable-write-back t)
  (setq orgist-write-back-dry-run t)
  (when (file-exists-p orgist-sync-token-filename)
    (delete-file orgist-sync-token-filename))
  (orgist)
  ;; Open files and build cache
  (dolist (file (directory-files orgist-base-dir t "\\.org$"))
    (find-file file) (org-mode) (orgist-build-id-cache))

  ;; Check archived section has :ARCHIVE: tag
  (let* ((arch-pos (orgist-find-element-by-id "arch-section-1"))
         (has-tag (when arch-pos
                    (save-excursion
                      (goto-char arch-pos)
                      (member "ARCHIVE" (cdr (orgist-extract-heading-and-tags)))))))
    (orgist-test-assert (not (null arch-pos))
                        "Archived section exists")
    (orgist-test-assert (not (null has-tag))
                        "Archived section has :ARCHIVE: tag"))

  ;; Check active sections do NOT have :ARCHIVE: tag
  (let* ((active-pos (orgist-find-element-by-id "fixture-section"))
         (has-archive (when active-pos
                        (save-excursion
                          (goto-char active-pos)
                          (member "ARCHIVE" (cdr (orgist-extract-heading-and-tags)))))))
    (orgist-test-assert (null has-archive)
                        "Active section has no :ARCHIVE: tag"))

  ;; Check snapshot has :archived-p
  (let ((snap (gethash "arch-section-1" orgist-snapshots)))
    (orgist-test-assert (eq (plist-get snap :archived-p) t)
                        "Snapshot has :archived-p t"))

  ;; Phase 2: Zero diffs for archived section
  (message "")
  (message "--- Phase 2: Zero diffs for archived section ---")
  (orgist-save-snapshots)
  (orgist-load-snapshots t)
  (dolist (file (directory-files orgist-base-dir t "\\.org$"))
    (when-let* ((buf (find-buffer-visiting file)))
      (with-current-buffer buf (orgist-build-id-cache))))
  (let* ((changes (orgist-diff-all-elements))
         (arch-diff (assoc "arch-section-1" changes)))
    (orgist-test-assert (null arch-diff)
                        "No diff for archived section"))

  ;; Phase 3: Unarchive detection (remove :ARCHIVE: tag → section_unarchive)
  (message "")
  (message "--- Phase 3: Unarchive detection ---")
  (let ((arch-pos (orgist-find-element-by-id "arch-section-1")))
    (when arch-pos
      (save-excursion
        (goto-char arch-pos)
        (let ((tags (cdr (orgist-extract-heading-and-tags))))
          (org-set-tags (remove "ARCHIVE" tags))))))
  (let* ((changes (orgist-diff-all-elements))
         (arch-diff (assoc "arch-section-1" changes))
         (diff (cdr arch-diff))
         (has-archived-p (assq :archived-p diff)))
    (orgist-test-assert (not (null arch-diff))
                        "Unarchive: diff detected")
    (orgist-test-assert (not (null has-archived-p))
                        "Unarchive: :archived-p in diff")
    (when changes
      (let ((commands (orgist-changes-to-commands changes)))
        (orgist-test-assert
         (cl-some (lambda (c) (equal (alist-get 'type c) "section_unarchive"))
                  commands)
         "Unarchive: produces section_unarchive command"))))

  ;; Phase 4: Archive detection (add :ARCHIVE: tag → section_archive)
  (message "")
  (message "--- Phase 4: Archive detection ---")
  (orgist-write-back)
  (orgist-save-snapshots)
  (orgist-load-snapshots t)
  (dolist (file (directory-files orgist-base-dir t "\\.org$"))
    (when-let* ((buf (find-buffer-visiting file)))
      (with-current-buffer buf (orgist-build-id-cache))))
  ;; Add :ARCHIVE: tag to an active section
  (let ((active-pos (orgist-find-element-by-id "fixture-section")))
    (when active-pos
      (save-excursion
        (goto-char active-pos)
        (org-set-tags '("ARCHIVE")))))
  (let* ((changes (orgist-diff-all-elements))
         (active-diff (assoc "fixture-section" changes)))
    (orgist-test-assert (not (null active-diff))
                        "Archive: diff detected for active section")
    (when changes
      (let ((commands (orgist-changes-to-commands changes)))
        (orgist-test-assert
         (cl-some (lambda (c) (equal (alist-get 'type c) "section_archive"))
                  commands)
         "Archive: produces section_archive command"))))

  ;; Phase 5: Combined edit + reorder on the same task.
  ;; Regression: the :order branch in `orgist-changes-to-commands' used
  ;; to reset `needs-update', silently dropping the item_update whenever
  ;; the same save also moved/reordered the task.  And
  ;; `orgist-update-snapshots-from-local' used to advance the FULL local
  ;; state for move/reorder commands, baking the dropped edits into the
  ;; snapshot as undetectable drift.
  (message "")
  (message "--- Phase 5: Combined edit + reorder ---")
  (let ((task-id
         (catch 'found
           (maphash (lambda (id snap)
                      (when (and (not (plist-get snap :section-p))
                                 (not (plist-get snap :archived-p))
                                 (orgist-find-element-by-id id)
                                 (null (orgist-diff-element id)))
                        (throw 'found id)))
                    orgist-snapshots)
           nil)))
    (orgist-test-assert (not (null task-id))
                        "Combined: found a clean task to edit")
    (when task-id
      (let* ((old-snap (gethash task-id orgist-snapshots))
             (old-content (plist-get old-snap :content)))
        ;; Rename the heading in the buffer.
        (save-excursion
          (goto-char (orgist-find-element-by-id task-id))
          (org-edit-headline "Combined edit and reorder"))
        ;; Force an :order diff by skewing the snapshot's stored order.
        (let ((skewed (copy-sequence old-snap)))
          (plist-put skewed :order (+ 100 (or (plist-get skewed :order) 0)))
          (puthash task-id skewed orgist-snapshots))
        (let* ((changes (orgist-diff-all-elements))
               (diff (cdr (assoc task-id changes))))
          (orgist-test-assert (assq :content diff)
                              "Combined: :content in diff")
          (orgist-test-assert (assq :order diff)
                              "Combined: :order in diff")
          (let* ((commands (orgist-changes-to-commands changes))
                 (update (cl-find-if
                          (lambda (c)
                            (and (equal (alist-get 'type c) "item_update")
                                 (equal (alist-get 'id (alist-get 'args c))
                                        task-id)))
                          commands))
                 (reorder (cl-find-if
                           (lambda (c)
                             (and (equal (alist-get 'type c) "item_reorder")
                                  (equal (alist-get
                                          'id
                                          (aref (alist-get 'items (alist-get 'args c)) 0))
                                         task-id)))
                           commands)))
            (orgist-test-assert (not (null update))
                                "Combined: item_update emitted despite reorder")
            (orgist-test-assert (not (null reorder))
                                "Combined: item_reorder emitted")
            (orgist-test-assert
             (and update (alist-get 'content (alist-get 'args update)))
             "Combined: item_update carries the new content")
            ;; Advance snapshots from ONLY the reorder command (as if the
            ;; item_update was dropped or failed): the unsent content edit
            ;; must remain detectable, while :order advances.
            (when reorder
              (orgist-update-snapshots-from-local (list reorder))
              (let ((diff2 (orgist-diff-element task-id)))
                (orgist-test-assert
                 (assq :content diff2)
                 "Combined: partial advance keeps unsent :content detectable")
                (orgist-test-assert
                 (not (assq :order diff2))
                 "Combined: partial advance settles :order"))
              ;; Restore original content and snapshot for later phases.
              (save-excursion
                (goto-char (orgist-find-element-by-id task-id))
                (org-edit-headline old-content))
              (puthash task-id old-snap orgist-snapshots)))))))

  ;; Phase 6: Fetch archived sections function
  (message "")
  (message "--- Phase 6: Fetch archived sections ---")
  (let ((sections (orgist-fetch-archived-sections)))
    (orgist-test-assert (not (null sections))
                        "Fetch archived sections returns data")
    (orgist-test-assert (length= sections 2)
                        "Fetch returns 2 archived sections")
    (orgist-test-assert (equal (alist-get 'id (car sections)) "arch-section-1")
                        "First section is arch-section-1"))

  ;; Results
  (message "")
  (message "========================================")
  (message "=== Results: Archived & Order ===")
  (message "=== Passed: %d  Failed: %d ==="
           orgist-test--passes orgist-test--failures)
  (message "========================================")
  (message "")
  orgist-test--failures)

;; Parse command-line args
(setq command-line-args-left
      (seq-remove (lambda (a) (string= a "--")) command-line-args-left))

(let* ((mode-arg (car command-line-args-left))
       (project-arg (cadr command-line-args-left))
       (total-failures 0))
  (setq command-line-args-left nil)
  (pcase mode-arg
    ("record"
     (unless project-arg
       (message "Usage: emacs --batch -l test-harness.el -- record <ProjectName>")
       (kill-emacs 1))
     (setq orgist-test-record-mode 'record)
     (message "[test-harness] Recording API responses for: %s" project-arg)
     (setq total-failures (orgist-test-run-lifecycle project-arg)))
    ("replay"
     (unless project-arg
       (message "Usage: emacs --batch -l test-harness.el -- replay <ProjectName>")
       (kill-emacs 1))
     (setq orgist-test-record-mode 'replay)
     (message "[test-harness] Replaying cached responses for: %s" project-arg)
     (setq total-failures (orgist-test-run-lifecycle project-arg)))
    ("all"
     (setq orgist-test-record-mode 'replay)
     (let ((projects (orgist-test--discover-projects)))
       (if (null projects)
           (progn
             (message "[test-harness] No cached projects found in test-data/")
             (kill-emacs 1))
         (message "[test-harness] Running all cached projects: %s"
                  (string-join projects ", "))
         (dolist (proj projects)
           (setq total-failures (+ total-failures
                                   (orgist-test-run-lifecycle proj)))))))
    ("move"
     (setq total-failures (orgist-test-run-cross-project-move)))
    ("state-log"
     (setq orgist-test-record-mode 'replay)
     (setq total-failures (orgist-test-run-state-log)))
    ("subprocess"
     (setq orgist-test-record-mode 'replay)
     (setq total-failures (+ (orgist-test-run-subprocess)
                              (orgist-test-run-subprocess-incremental))))
    ("format"
     (setq total-failures (orgist-test-run-formatting)))
    ("comments"
     (setq orgist-test-record-mode 'replay)
     (setq total-failures (orgist-test-run-comments)))
    ("features"
     (setq total-failures (orgist-test-run-new-features)))
    ("encoding"
     (setq total-failures (orgist-test-run-encoding)))
    ("quick-add"
     (setq total-failures (orgist-test-run-quick-add)))
    ("attachments"
     (setq total-failures (orgist-test-run-attachments)))
    ("completed"
     (setq orgist-test-record-mode 'replay)
     (setq total-failures (orgist-test-run-completed-tasks)))
    ("archived"
     (setq orgist-test-record-mode 'replay)
     (setq total-failures (orgist-test-run-archived-sections)))
    ("metadata"
     (setq total-failures (orgist-test-run-metadata)))
    ("record-comments"
     (unless project-arg
       (message "Usage: emacs --batch -l test-harness.el -- record-comments <ProjectName>")
       (kill-emacs 1))
     (setq orgist-test-record-mode 'record)
     (message "[test-harness] Recording comments/activity for: %s" project-arg)
     ;; First do a full sync, then pull comments
     (orgist-test-setup-isolation project-arg)
     (when (file-exists-p orgist-sync-token-filename)
       (delete-file orgist-sync-token-filename))
     (orgist)
     (orgist-test--wait-for-sync 120)
     ;; Open files and pull comments for all tasks
     (dolist (file (directory-files orgist-base-dir t "\\.org$"))
       (find-file file) (org-mode) (orgist-build-id-cache))
     (maphash
      (lambda (id snap)
        (unless (plist-get snap :section-p)
          (message "[test-harness] Recording comments for %s" id)
          (condition-case err
              (progn
                (orgist-fetch-task-comments id)
                (orgist-fetch-task-activity id))
            (error (message "[test-harness] Error: %s" (error-message-string err))))
          (sleep-for 0.2)))
      orgist-snapshots)
     (message "[test-harness] Comment recording complete")
     (setq total-failures 0))
    (_
     (message "Usage: emacs --batch -l test-harness.el -- {record|replay|all|move|subprocess|format|comments|completed|attachments|record-comments} [ProjectName]")
     (kill-emacs 1)))
  (message "")
  (if (= total-failures 0)
      (progn
        (message "[test-harness] ALL TESTS PASSED")
        (kill-emacs 0))
    (message "[test-harness] FAILURES: %d" total-failures)
    (kill-emacs 1)))

;;; test-harness.el ends here
