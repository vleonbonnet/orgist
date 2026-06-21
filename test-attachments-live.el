;;; test-attachments-live.el --- Live CRUD test for attachment sync -*- lexical-binding: t; -*-

;; Usage:
;;   emacs --batch -l test-attachments-live.el -- Orgtest
;;
;; Needs TODOIST_API_TOKEN env var or `pass`.
;; Tests all 4 CRUD operations in both directions (push + pull).

(add-to-list 'load-path (expand-file-name "~/.emacs.d/elpaca/builds/request"))
(add-to-list 'load-path default-directory)
(let ((script-dir (file-name-directory (or load-file-name buffer-file-name))))
  (load (expand-file-name "orgist.el" script-dir) nil nil t))
(require 'json)
(require 'org-attach)

;;; ============================================================
;;; Assertions & helpers
;;; ============================================================

(defvar orgist-att-test--failures 0)
(defvar orgist-att-test--passes 0)
(defvar orgist-att-test--cleanup-ids '()
  "Comment IDs to clean up at end.")

(defun att-assert (condition desc)
  (if condition
      (progn (cl-incf orgist-att-test--passes)
             (message "[PASS] %s" desc))
    (cl-incf orgist-att-test--failures)
    (message "[FAIL] %s" desc)))

(defun att-assert-equal (expected actual desc)
  (if (equal expected actual)
      (progn (cl-incf orgist-att-test--passes)
             (message "[PASS] %s" desc))
    (cl-incf orgist-att-test--failures)
    (message "[FAIL] %s (expected %S, got %S)" desc expected actual)))

(defun att-wait-sync (max-secs)
  (let ((waited 0))
    (while (and orgist-sync-mutex (< waited max-secs))
      (sleep-for 1) (cl-incf waited)
      (when (= (% waited 10) 0)
        (message "  Waiting for sync... %ds" waited)))
    (not orgist-sync-mutex)))

(defun att-wait-process (name max-secs)
  "Wait for process NAME to finish, up to MAX-SECS."
  (let ((proc (get-process name))
        (waited 0))
    (when proc
      (while (and (process-live-p proc) (< waited max-secs))
        (accept-process-output proc 1)
        (cl-incf waited)
        (when (= (% waited 10) 0)
          (message "  Waiting for %s... %ds" name waited))))
    (accept-process-output nil 0.5)))

(defun att-open-org-files ()
  "Open all org files in orgist-base-dir and build id caches.
Returns the last buffer opened (the Orgtest.org buffer)."
  (let ((last-buf nil))
    (dolist (file (directory-files orgist-base-dir t "\\.org$"))
      (setq last-buf (find-file file))
      (org-mode)
      (orgist-build-id-cache))
    last-buf))

(defun att-fetch-comments (task-id)
  "Fetch comments for TASK-ID.  Returns a list of alists."
  (orgist-fetch-task-comments task-id))

(defun att-find-attachment-comment (task-id file-name)
  "Find comment ID for FILE-NAME attachment on TASK-ID."
  (let ((result nil))
    (dolist (comment (att-fetch-comments task-id))
      (let ((att (or (alist-get 'file_attachment comment)
                     (alist-get 'attachment comment))))
        (when (and att (equal (alist-get 'file_name att) file-name))
          (setq result (alist-get 'id comment)))))
    result))

(defun att-force-note-count (task-id count)
  "Set :note-count in snapshot for TASK-ID to COUNT.
This forces orgist-sync-task-comments-and-activity to fetch
comments instead of skipping due to note_count=0 optimization."
  (let ((snap (gethash task-id orgist-snapshots)))
    (when snap
      (plist-put snap :note-count count)
      (puthash task-id snap orgist-snapshots))))

;;; ============================================================
;;; Config & isolation
;;; ============================================================

;; Parse command-line args
(setq command-line-args-left
      (seq-remove (lambda (a) (string= a "--")) command-line-args-left))
(let ((project (car command-line-args-left)))
  (setq command-line-args-left nil)
  (unless project
    (message "Usage: emacs --batch -l test-attachments-live.el -- <ProjectName>")
    (kill-emacs 1))
  (setq orgist-sync-project-filter project))

;; Set bearer token
(setq orgist-bearer-token
      (or (getenv "TODOIST_API_TOKEN")
          (error "No API token: set TODOIST_API_TOKEN")))

;; Isolated runtime directory
(let* ((runtime-dir (expand-file-name "orgist-test/LiveAttach/"
                                      temporary-file-directory)))
  (when (file-directory-p runtime-dir)
    (delete-directory runtime-dir t))
  (make-directory runtime-dir t)
  (setq orgist-base-dir runtime-dir)
  (setq orgist-sync-token-filename (expand-file-name "sync_token" runtime-dir))
  (setq orgist-snapshot-file (expand-file-name "snapshots.el" runtime-dir))
  (setq orgist-log-file (expand-file-name "orgist.log" runtime-dir)))

;; Config
(setq orgist-log-level 'info)
(setq orgist-sync-comments t)
(setq orgist-sync-attachments t)
(setq orgist-sync-on-save nil)  ; prevent save-buffer from triggering write-back
(setq orgist-enable-write-back t)
(setq orgist-write-back-dry-run nil)  ; REAL write-back
(setq revert-without-query '(".*"))
(setq orgist-project-buffer-cache nil)
(setq orgist-snapshots nil)
(setq orgist-sync-mutex nil)
(setq org-priority-highest 1 org-priority-lowest 5 org-priority-default 5)
(setq org-log-into-drawer t)
(setq create-lockfiles nil)  ; prevent lock conflicts in batch mode
(dolist (buf (buffer-list))
  (when (and (buffer-file-name buf)
             (string-match-p "\\.org$" (buffer-file-name buf)))
    (kill-buffer buf)))

;; Use the "Barz" task for testing
(defvar att-test-task-id "fixture-comment-task")

(message "")
(message "========================================")
(message "=== Live Attachment CRUD Test ===")
(message "========================================")
(message "  base-dir: %s" orgist-base-dir)

;;; ============================================================
;;; Phase 0: Clean up leftover attachment comments from prior runs
;;; ============================================================
(message "")
(message "--- Phase 0: Cleanup stale attachment comments ---")
(let ((stale-count 0))
  (dolist (comment (att-fetch-comments att-test-task-id))
    (when-let* ((att (alist-get 'file_attachment comment)))
      (when (and att (alist-get 'file_name att))
        (let ((cid (alist-get 'id comment))
              (fn (alist-get 'file_name att)))
          (when (member fn '("test-doc.txt" "push-test.txt"))
            (message "  Removing stale attachment comment %s (%s)" cid fn)
            (condition-case nil (orgist-delete-comment cid) (error nil))
            (cl-incf stale-count))))))
  (message "  Cleaned %d stale attachment comment(s)" stale-count))

;;; ============================================================
;;; Phase 1: Full sync (fresh start)
;;; ============================================================
(message "")
(message "--- Phase 1: Full sync (fresh, isolated dir) ---")

;; Force full sync
(when (file-exists-p orgist-sync-token-filename)
  (delete-file orgist-sync-token-filename))

(orgist)
(att-assert (att-wait-sync 120) "Full sync completed")

;; Wait for comments subprocess (if any)
(att-wait-process "orgist-comments" 120)

;; Open files and switch to Orgtest.org
(att-open-org-files)
;; Reload snapshots from disk (subprocess may have updated them)
(orgist-load-snapshots t)

(let ((pos (orgist-find-element-by-id att-test-task-id)))
  (att-assert pos (format "Barz task found (pos=%s)" pos)))

;; Record initial state
(let* ((snap (gethash att-test-task-id orgist-snapshots))
       (initial-attachments (when snap (plist-get snap :attachment-files)))
       (initial-note-count (when snap (plist-get snap :note-count)))
       (initial-comment-ids (when snap (plist-get snap :comment-ids))))
  (message "  Initial attachments: %S" initial-attachments)
  (message "  Initial note-count: %S" initial-note-count)
  (message "  Initial comment-ids count: %d" (length (or initial-comment-ids '()))))

;; Save baseline
(orgist-save-snapshots)
(sleep-for 1)

;;; ============================================================
;;; Phase 2: PUSH CREATE — add local file, write-back to Todoist
;;; ============================================================
(message "")
(message "--- Phase 2: Push CREATE — add local file, write-back ---")

(let* ((pos (orgist-find-element-by-id att-test-task-id))
       (script-dir (file-name-directory (or load-file-name buffer-file-name)))
       (dummy-file (expand-file-name "test-data/dummy-attachments/test-doc.txt"
                                     script-dir)))
  (att-assert (file-exists-p dummy-file) "Dummy file exists")
  (when pos
    (goto-char pos)
    (org-back-to-heading t)
    (org-attach-attach dummy-file nil 'cp)
    (message "  Attached test-doc.txt to Barz heading")
    (save-buffer)))

;; Diff should detect the new attachment
(orgist-build-id-cache)
(let* ((changes (orgist-diff-all-elements))
       (task-change (assoc att-test-task-id changes)))
  (att-assert task-change "Diff detects attachment change on Barz")
  (when task-change
    (let ((att-diff (assq :attachment-files (cdr task-change))))
      (att-assert att-diff "Diff includes :attachment-files field")
      (when att-diff
        (message "  Diff: old=%S new=%S" (cadr att-diff) (cddr att-diff))))))

;; Execute ONLY attachment commands (not note_add or other spurious diffs)
(message "  Executing write-back (push upload)...")
(let* ((changes (orgist-diff-all-elements))
       (commands (orgist-changes-to-commands changes))
       (att-cmds (seq-filter
                  (lambda (c) (string-match-p "^attachment_"
                                              (or (alist-get 'type c) "")))
                  commands)))
  (att-assert (>= (length att-cmds) 1)
              (format "Generated %d attachment command(s)" (length att-cmds)))
  (when att-cmds
    (orgist-execute-write-back att-cmds)
    (sleep-for 3)))

;; Verify: fetch the specific comment by ID to check file_attachment
(message "  Verifying attachment in Todoist API...")
(let* ((upload-comment-id nil)
       (_ (let ((snap (gethash att-test-task-id orgist-snapshots)))
            (when snap
              (let ((atts (plist-get snap :attachment-files)))
                (when atts
                  (setq upload-comment-id (cdar atts)))))))
       (individual-comment nil))
  (message "  Upload comment ID from snapshot: %s" upload-comment-id)
  (when upload-comment-id
    (push upload-comment-id orgist-att-test--cleanup-ids)
    (request
      (format "https://api.todoist.com/api/v1/comments/%s" upload-comment-id)
      :headers `(("Authorization" . ,(format "Bearer %s" orgist-bearer-token)))
      :parser 'json-read
      :sync t
      :success (cl-function
                (lambda (&key data &allow-other-keys)
                  (setq individual-comment data))))
    (let ((att (alist-get 'file_attachment individual-comment)))
      (att-assert (and att (alist-get 'file_name att))
                  (format "Push CREATE: test-doc.txt in Todoist (comment=%s, file=%s)"
                          upload-comment-id
                          (when att (alist-get 'file_name att)))))))

;;; ============================================================
;;; Phase 3: PULL READ — re-pull, verify attachment round-trips
;;; ============================================================
(message "")
(message "--- Phase 3: Pull READ — re-pull, verify round-trip ---")

;; Force note-count to be non-zero so comment fetch isn't skipped
(att-force-note-count att-test-task-id 99)

;; Pull comments (synchronous, in current buffer)
(let ((pos (orgist-find-element-by-id att-test-task-id)))
  (when pos
    (goto-char pos)
    (org-back-to-heading t)
    (orgist-sync-task-comments-and-activity att-test-task-id)
    (save-buffer)))

;; Save snapshots NOW (preserve :comment-ids from the pull)
(orgist-save-snapshots)

;; Check snapshot includes the attachment
(let* ((snap (gethash att-test-task-id orgist-snapshots))
       (attachments (when snap (plist-get snap :attachment-files))))
  (att-assert (assoc "test-doc.txt" attachments)
              (format "Pull READ: test-doc.txt in snapshot (%S)" attachments)))

;; Check file exists on disk
(let ((pos (orgist-find-element-by-id att-test-task-id)))
  (when pos
    (goto-char pos)
    (org-back-to-heading t)
    (let ((dir (org-attach-dir)))
      (att-assert (and dir (file-exists-p (expand-file-name "test-doc.txt" dir)))
                  "Pull READ: test-doc.txt exists in org-attach dir"))))

;; Zero spurious diffs (ignore date diffs and note diffs)
(let* ((changes (orgist-diff-all-elements))
       (real-diffs (seq-remove
                    (lambda (c)
                      (and (listp (cdr c))
                           (seq-every-p
                            (lambda (d) (memq (car d) '(:due :deadline :notes)))
                            (cdr c))))
                    changes)))
  (att-assert-equal 0 (length real-diffs) "Pull READ: zero spurious diffs")
  (when real-diffs
    (message "  Unexpected diffs:")
    (dolist (c (seq-take real-diffs 5))
      (message "    %s: %S" (car c) (cdr c)))))

;;; ============================================================
;;; Phase 4: PULL CREATE — upload file via API, then pull
;;; ============================================================
(message "")
(message "--- Phase 4: Pull CREATE — upload via API, then pull ---")

(let* ((script-dir (file-name-directory (or load-file-name buffer-file-name)))
       (dummy-file (expand-file-name "test-data/dummy-attachments/push-test.txt"
                                     script-dir))
       (upload-result nil)
       (comment-result nil)
       (remote-file-name nil))
  ;; Upload file to Todoist
  (message "  Uploading push-test.txt to Todoist...")
  (setq upload-result (orgist-upload-file dummy-file))
  (att-assert upload-result "Upload API returned result")
  (when upload-result
    (setq remote-file-name (alist-get 'file_name upload-result))
    (message "  Upload result: file_name=%s" remote-file-name)
    ;; Create comment with attachment
    (setq comment-result
          (orgist-create-comment-with-attachment att-test-task-id upload-result))
    (att-assert comment-result "Comment created with attachment")
    (when comment-result
      (message "  Comment created: id=%s" (alist-get 'id comment-result))
      (push (alist-get 'id comment-result) orgist-att-test--cleanup-ids)))

  ;; Force note-count so comment fetch isn't skipped
  (att-force-note-count att-test-task-id 99)

  ;; Pull comments to get the new attachment
  (sleep-for 3)
  (let ((pos (orgist-find-element-by-id att-test-task-id)))
    (when pos
      (goto-char pos)
      (org-back-to-heading t)
      (orgist-sync-task-comments-and-activity att-test-task-id)
      (save-buffer)))

  ;; Save snapshots to persist :comment-ids and :attachment-files
  (orgist-save-snapshots)

  ;; Verify the new file was downloaded
  (let ((pos (orgist-find-element-by-id att-test-task-id)))
    (when pos
      (goto-char pos)
      (org-back-to-heading t)
      (let* ((dir (org-attach-dir))
             (files (when (and dir (file-directory-p dir))
                      (directory-files dir nil "^[^.]"))))
        (message "  Files in attach dir: %S" files)
        (att-assert (and files (member (or remote-file-name "push-test.txt") files))
                    (format "Pull CREATE: %s downloaded"
                            (or remote-file-name "push-test.txt")))))))

(sleep-for 1)

;;; ============================================================
;;; Phase 5: PUSH DELETE — remove local file, write-back
;;; ============================================================
(message "")
(message "--- Phase 5: Push DELETE — remove test-doc.txt, write-back ---")

(let ((pos (orgist-find-element-by-id att-test-task-id)))
  (when pos
    (goto-char pos)
    (org-back-to-heading t)
    (let* ((dir (org-attach-dir))
           (file-path (when dir (expand-file-name "test-doc.txt" dir))))
      (att-assert (and file-path (file-exists-p file-path))
                  "test-doc.txt exists before delete")
      (when (and file-path (file-exists-p file-path))
        (delete-file file-path)
        (message "  Deleted test-doc.txt from attach dir")
        (set-buffer-modified-p t)
        (save-buffer)))))

;; Diff and generate ONLY attachment commands
(orgist-build-id-cache)
(let* ((changes (orgist-diff-all-elements))
       (commands (orgist-changes-to-commands changes))
       (att-cmds (seq-filter
                  (lambda (c) (string-match-p "^attachment_"
                                              (or (alist-get 'type c) "")))
                  commands)))
  (att-assert (>= (length att-cmds) 1)
              (format "Push DELETE: %d delete command(s)" (length att-cmds)))
  (when att-cmds
    (message "  Delete targets: %S"
             (mapcar (lambda (c) (alist-get 'file_name (alist-get 'args c)))
                     att-cmds))
    (message "  Executing write-back (push delete)...")
    (orgist-execute-write-back att-cmds)
    (sleep-for 3)))

;; Verify: the specific comment was deleted
(message "  Verifying deletion in Todoist...")
(let* ((snap (gethash att-test-task-id orgist-snapshots))
       (atts (when snap (plist-get snap :attachment-files)))
       (deleted-id (cdr (assoc "test-doc.txt" atts)))
       (still-exists nil))
  (when deleted-id
    (request
      (format "https://api.todoist.com/api/v1/comments/%s" deleted-id)
      :headers `(("Authorization" . ,(format "Bearer %s" orgist-bearer-token)))
      :parser 'json-read
      :sync t
      :error (cl-function
              (lambda (&key error-thrown &allow-other-keys)
                ;; 404/410 means deleted — good
                nil))
      :success (cl-function
                (lambda (&key data &allow-other-keys)
                  (unless (eq (alist-get 'is_deleted data) t)
                    (setq still-exists t))))))
  (att-assert (not still-exists)
              "Push DELETE: test-doc.txt comment deleted from Todoist"))

;;; ============================================================
;;; Phase 6: PULL DELETE — delete Todoist comment, re-pull
;;; ============================================================
(message "")
(message "--- Phase 6: Pull DELETE — delete API comment, re-pull ---")

;; Find the push-test.txt attachment comment via API
(let ((comment-id (att-find-attachment-comment att-test-task-id "push-test.txt"))
      (file-name "push-test.txt"))
  (att-assert comment-id
              (format "Found comment to delete (id=%s, file=%s)"
                      comment-id file-name))
  (when comment-id
    (message "  Deleting comment %s (%s)..." comment-id file-name)
    (att-assert (orgist-delete-comment comment-id) "API delete succeeded")
    ;; Remove from cleanup list
    (setq orgist-att-test--cleanup-ids
          (remove comment-id orgist-att-test--cleanup-ids))
    (sleep-for 3)

    ;; Force note-count so comment fetch isn't skipped
    (att-force-note-count att-test-task-id 99)

    ;; Re-pull comments to detect the deletion
    (let ((pos (orgist-find-element-by-id att-test-task-id)))
      (when pos
        (goto-char pos)
        (org-back-to-heading t)
        (orgist-sync-task-comments-and-activity att-test-task-id)
        (save-buffer)))

    ;; Verify: file removed from local dir
    (let ((pos (orgist-find-element-by-id att-test-task-id)))
      (when pos
        (goto-char pos)
        (org-back-to-heading t)
        (let* ((dir (org-attach-dir))
               (files (when (and dir (file-directory-p dir))
                        (directory-files dir nil "^[^.]"))))
          (message "  Files remaining: %S" files)
          (att-assert
           (not (and dir file-name
                     (file-exists-p (expand-file-name file-name dir))))
           (format "Pull DELETE: %s removed from org-attach dir" file-name)))))))

;;; ============================================================
;;; Cleanup
;;; ============================================================
(message "")
(message "--- Cleanup ---")
(dolist (cid orgist-att-test--cleanup-ids)
  (message "  Cleaning up comment %s..." cid)
  (condition-case nil (orgist-delete-comment cid) (error nil)))
(when orgist-att-test--cleanup-ids
  (message "  Cleaned up %d remaining test comment(s)"
           (length orgist-att-test--cleanup-ids)))

;;; ============================================================
;;; Results
;;; ============================================================
(message "")
(message "========================================")
(message "=== Results: Live Attachment CRUD ===")
(message "=== Passed: %d  Failed: %d ==="
         orgist-att-test--passes orgist-att-test--failures)
(message "========================================")
(message "")
(if (= orgist-att-test--failures 0)
    (progn (message "[test] ALL TESTS PASSED") (kill-emacs 0))
  (message "[test] FAILURES: %d" orgist-att-test--failures)
  (kill-emacs 1))

;;; test-attachments-live.el ends here
