;;; test-repeat-log.el --- Regression tests for recurring repeat entries -*- lexical-binding: t; -*-

;; Usage: emacs --batch -L . -l ert -l test-repeat-log.el -f ert-run-tests-batch-and-exit
;;
;; A pull logged a "TODO -> TODO" repeat entry whenever a recurring
;; task's date changed, so moving the date of "Renouveler mon
;; passeport" in Todoist read as a completion in its LOGBOOK.  Todoist
;; adds one to `completed_count' on each completion and leaves it alone
;; on a reschedule.  The snapshot now records the count, a pull logs a
;; repeat only when the count rose, and the fetch that follows an
;; `item_close' records the count that close raised, so the echoing
;; pull does not log the completion a second time.

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

(defconst orgist-test--repeat-project-file
  "* Chores
:PROPERTIES:
:ID:       6Xsec
:SECTION:
:END:
** TODO Water the plants
SCHEDULED: <2026-07-20 Mon +1w>
:PROPERTIES:
:ID:       6Xrep
:TODOIST_DUE_STRING: every week
:END:
")

(defmacro orgist-test--with-repeat-project (count &rest body)
  "Run BODY in a throwaway project whose recurring task is snapshotted.
COUNT is the `completed_count' the snapshot records; nil records none,
as a snapshot written before the field existed."
  (declare (indent 1))
  `(let* ((dir (make-temp-file "orgist-test-" t))
          (file (expand-file-name "Proj.org" dir))
          (orgist-base-dir dir)
          (orgist-snapshot-file (expand-file-name "snapshots.el" dir))
          (orgist-log-file nil)
          (orgist--write-back-stamps nil)
          (orgist--write-back-stamps-path nil)
          (orgist--pending-stamps nil)
          (orgist-snapshots (make-hash-table :test 'equal))
          (orgist-reminders nil)
          (orgist-labels nil)
          (orgist-sync-comments nil)
          (orgist-sync-attachments nil)
          (orgist-enable-write-back t)
          (orgist-project-buffer-cache nil))
     (with-temp-file file (insert orgist-test--repeat-project-file))
     (let ((buf (find-file-noselect file)))
       (unwind-protect
           (with-current-buffer buf
             (orgist-build-id-cache)
             (goto-char (orgist-find-element-by-id "6Xrep"))
             (let ((snapshot (orgist-element-local-state)))
               (when ,count
                 (setq snapshot (plist-put snapshot :completed-count ,count)))
               (puthash "6Xrep" snapshot orgist-snapshots))
             ,@body)
         (with-current-buffer buf
           (set-buffer-modified-p nil))
         (kill-buffer buf)
         (delete-directory dir t)))))

(defun orgist-test--repeat-element (date &optional count)
  "Todoist's recurring task due on DATE, with `completed_count' COUNT."
  `((id . "6Xrep") (content . "Water the plants") (checked . :json-false)
    (priority . 1) (project_id . "PROJX") (section_id . "6Xsec")
    (description . "")
    (due . ((date . ,date) (is_recurring . t) (string . "every week")))
    ,@(when count `((completed_count . ,count)))))

(defun orgist-test--pull-repeat (element)
  "Apply ELEMENT to the recurring task, as a pull does."
  (goto-char (orgist-find-element-by-id "6Xrep"))
  (orgist-update-element element))

(defun orgist-test--repeat-entries ()
  "Count the TODO -> TODO entries of the recurring task."
  (goto-char (orgist-find-element-by-id "6Xrep"))
  (let ((end (save-excursion (outline-next-heading) (point)))
        (count 0))
    (while (re-search-forward "^- .*\"TODO\".*\"TODO\"" end t)
      (setq count (1+ count)))
    count))

(ert-deftest orgist-repeat-log/reschedule-logs-no-repeat ()
  "A new date with an unchanged completion count is a reschedule."
  (orgist-test--with-repeat-project 3
    (orgist-test--pull-repeat (orgist-test--repeat-element "2026-07-22" 3))
    (should (= 0 (orgist-test--repeat-entries)))
    (should (equal "<2026-07-22 Wed +1w>"
                   (org-entry-get (orgist-find-element-by-id "6Xrep") "SCHEDULED")))))

(ert-deftest orgist-repeat-log/completion-logs-repeat ()
  "A new date with a higher completion count is a completion."
  (orgist-test--with-repeat-project 3
    (orgist-test--pull-repeat (orgist-test--repeat-element "2026-07-27" 4))
    (should (= 1 (orgist-test--repeat-entries)))))

(ert-deftest orgist-repeat-log/unknown-count-keeps-date-rule ()
  "Without a recorded count, a new date still logs a repeat."
  (orgist-test--with-repeat-project nil
    (orgist-test--pull-repeat (orgist-test--repeat-element "2026-07-27" 4))
    (should (= 1 (orgist-test--repeat-entries)))))

(ert-deftest orgist-repeat-log/pull-records-count ()
  "A pull records Todoist's completion count in the snapshot."
  (orgist-test--with-repeat-project nil
    (orgist-test--pull-repeat (orgist-test--repeat-element "2026-07-27" 4))
    (should (equal 4 (plist-get (gethash "6Xrep" orgist-snapshots)
                                :completed-count)))))

(ert-deftest orgist-repeat-log/close-echo-logs-no-repeat ()
  "The pull echoing an `item_close' does not log the completion again."
  (orgist-test--with-repeat-project 3
    (cl-letf (((symbol-function 'orgist-remote)
               (cl-function
                (lambda (operation &key path success &allow-other-keys)
                  (should (eq operation 'get-task))
                  (should (equal path '("6Xrep")))
                  (funcall success
                           :data '((id . "6Xrep") (completed_count . 4)
                                   (due . ((date . "2026-07-27") (is_recurring . t)
                                           (string . "every week")))))))))
      (orgist--refresh-recurring-dates '("6Xrep")))
    (should (equal 4 (plist-get (gethash "6Xrep" orgist-snapshots)
                                :completed-count)))
    (orgist-test--pull-repeat (orgist-test--repeat-element "2026-07-27" 4))
    (should (= 0 (orgist-test--repeat-entries)))))

;;; test-repeat-log.el ends here
