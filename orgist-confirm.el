;;; orgist-confirm.el --- Write-back confirmation buffer -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2026 Valentin Leon

;; This file is part of orgist.

;;; Commentary:

;; Provides a dedicated major mode for reviewing pending write-back
;; changes in a collapsible tree (project > task > field) before
;; sending them to the Todoist API.
;;
;; Two-level expansion:
;;   Collapsed project  ▶ Orgtest  2 modified, 1 deleted
;;   Expanded project   ▼ Orgtest  2 modified, 1 deleted
;;                        ▶ ✎ Barz  title, description
;;                        ▶ ✎ Foo   priority
;;                          ✗ Old task  deleted
;;   Expanded task        ▼ ✎ Barz  title, description
;;                            title:        "Barz" → "Barz updated"
;;                            description:  "old text…" → "new text…"

;;; Code:

(require 'cl-lib)

;; Forward declarations for orgist.el functions (avoids circular require)
(declare-function orgist-execute-write-back "orgist" (commands))
(declare-function orgist-log "orgist" (level format-string &rest args))
(declare-function orgist-group-changes-by-project "orgist" (changes))
(defvar orgist-sync-mutex)

;;; Faces

(defface orgist-confirm-title
  '((t :weight bold :height 1.2))
  "Face for the confirmation buffer title."
  :group 'orgist)

(defface orgist-confirm-project
  '((t :weight bold :inherit font-lock-keyword-face))
  "Face for project names."
  :group 'orgist)

(defface orgist-confirm-task
  '((t :inherit font-lock-function-name-face))
  "Face for task names."
  :group 'orgist)

(defface orgist-confirm-field-name
  '((t :inherit font-lock-variable-name-face))
  "Face for field labels."
  :group 'orgist)

(defface orgist-confirm-old-value
  '((t :foreground "#cc4444"))
  "Face for old (replaced) values."
  :group 'orgist)

(defface orgist-confirm-new-value
  '((t :foreground "#44aa44"))
  "Face for new values."
  :group 'orgist)

(defface orgist-confirm-deleted
  '((t :foreground "#cc4444" :weight bold))
  "Face for deleted tasks."
  :group 'orgist)

(defface orgist-confirm-new-task
  '((t :foreground "#44aa44" :weight bold))
  "Face for new tasks."
  :group 'orgist)

(defface orgist-confirm-summary
  '((t :inherit font-lock-comment-face))
  "Face for summary counts."
  :group 'orgist)

(defface orgist-confirm-key
  '((t :weight bold :inherit font-lock-builtin-face))
  "Face for keybinding hints."
  :group 'orgist)

;;; Buffer-local state

(defvar-local orgist-confirm--commands nil
  "Commands to execute on confirm.")

(defvar-local orgist-confirm--continuation nil
  "Function to call after confirm to continue the sync flow.")

(defvar-local orgist-confirm--nodes nil
  "List of collapsible nodes.
Each node is a plist (:marker MARKER :overlay OVERLAY :level LEVEL)
where LEVEL is `project' or `task'.")

;;; Entry point

(defun orgist-confirm-show (changes commands &optional continuation)
  "Display confirmation buffer for write-back CHANGES/COMMANDS.
CHANGES is the list of (ID . DIFF) pairs from `orgist-diff-all-elements'.
COMMANDS is the list of Sync API commands to execute on confirm.
CONTINUATION, if non-nil, is called after the user confirms to
continue the sync flow.  It is NOT called on cancel."
  (let* ((grouped (orgist-group-changes-by-project changes))
         (buf (get-buffer-create "*orgist-confirm*"))
         (total (length changes)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        ;; Set mode BEFORE rendering so kill-all-local-variables
        ;; doesn't wipe out buffer-local state set during rendering.
        (orgist-confirm-mode)
        (orgist-confirm--insert-header total)
        (dolist (group grouped)
          (orgist-confirm--insert-project (car group) (cdr group)))
        (orgist-confirm--insert-footer))
      (goto-char (point-min))
      (setq orgist-confirm--commands commands)
      (setq orgist-confirm--continuation continuation))
    (pop-to-buffer buf)))

;;; Rendering

(defun orgist-confirm--insert-header (total-count)
  "Insert the buffer header showing TOTAL-COUNT changes."
  (insert (propertize (format "Orgist Write-Back — %d change(s)" total-count)
                      'face 'orgist-confirm-title)
          "\n"
          (make-string 34 ?═)
          "\n\n"))

(defun orgist-confirm--insert-project (project-name items)
  "Insert a collapsible project section for PROJECT-NAME with ITEMS."
  (let* ((counts (orgist-confirm--count-types items))
         (summary-parts '())
         (header-start (point)))
    (when (> (alist-get 'modified counts 0) 0)
      (push (format "%d modified" (alist-get 'modified counts)) summary-parts))
    (when (> (alist-get 'deleted counts 0) 0)
      (push (format "%d deleted" (alist-get 'deleted counts)) summary-parts))
    (when (> (alist-get 'new counts 0) 0)
      (push (format "%d new" (alist-get 'new counts)) summary-parts))
    ;; Project header line (with toggle arrow)
    (insert (propertize "▼" 'orgist-confirm-arrow t)
            " "
            (propertize project-name 'face 'orgist-confirm-project)
            (propertize (concat "  " (string-join (nreverse summary-parts) ", "))
                        'face 'orgist-confirm-summary)
            "\n")
    ;; Task lines (initially visible)
    (let ((body-start (point)))
      (dolist (item items)
        (orgist-confirm--insert-task
         (nth 0 item) (nth 1 item) (nth 2 item) (nth 3 item)))
      (insert "\n")
      ;; Create overlay for the project body (task list)
      (let ((ov (make-overlay body-start (point))))
        (overlay-put ov 'invisible nil)
        (overlay-put ov 'orgist-confirm-node t)
        (push (list :marker (copy-marker header-start)
                    :overlay ov
                    :level 'project)
              orgist-confirm--nodes)))))

(defun orgist-confirm--insert-task (_id name diff-type diff)
  "Insert a task line for element NAME with DIFF-TYPE and DIFF."
  (pcase diff-type
    ('deleted
     (insert "  "
             (propertize "✗" 'face 'orgist-confirm-deleted)
             " "
             (propertize name 'face 'orgist-confirm-deleted)
             (propertize "  deleted" 'face 'orgist-confirm-summary)
             "\n"))
    ('new
     (insert "  "
             (propertize "+" 'face 'orgist-confirm-new-task)
             " "
             (propertize name 'face 'orgist-confirm-new-task)
             (propertize "  new" 'face 'orgist-confirm-summary)
             "\n"))
    ('modified
     (let* ((field-names (mapcar (lambda (fc)
                                   (let ((key (car fc)))
                                     (if (eq key :content) "title"
                                       (substring (symbol-name key) 1))))
                                 diff))
            (header-start (point)))
       ;; Task header line (expandable if there are field details)
       (insert "  "
               (propertize "▶" 'orgist-confirm-arrow t)
               " "
               (propertize "✎" 'face 'orgist-confirm-task)
               " "
               (propertize name 'face 'orgist-confirm-task)
               (propertize (concat "  " (string-join field-names ", "))
                           'face 'orgist-confirm-summary)
               "\n")
       ;; Field detail lines (initially hidden)
       (let ((detail-start (point)))
         (dolist (field-change diff)
           (orgist-confirm--insert-field
            (car field-change) (cadr field-change) (cddr field-change)))
         ;; Create overlay for field details
         (let ((ov (make-overlay detail-start (point))))
           (overlay-put ov 'invisible t)
           (overlay-put ov 'orgist-confirm-node t)
           (push (list :marker (copy-marker header-start)
                       :overlay ov
                       :level 'task)
                 orgist-confirm--nodes)))))))

(defun orgist-confirm--insert-field (field old new)
  "Insert a detail line for FIELD showing OLD → NEW values."
  (let ((field-name (if (eq field :content) "title"
                      (substring (symbol-name field) 1)))
        (old-str (orgist-confirm--format-value field old))
        (new-str (orgist-confirm--format-value field new)))
    (insert "    "
            (propertize (format "%-14s" (concat field-name ":"))
                        'face 'orgist-confirm-field-name)
            (propertize old-str 'face 'orgist-confirm-old-value)
            " → "
            (propertize new-str 'face 'orgist-confirm-new-value)
            "\n")))

(defun orgist-confirm--insert-footer ()
  "Insert the keybinding footer."
  (insert "\n"
          (propertize "C-c C-c" 'face 'orgist-confirm-key)
          "  Confirm    "
          (propertize "C-c C-k" 'face 'orgist-confirm-key)
          "  Cancel    "
          (propertize "TAB" 'face 'orgist-confirm-key)
          "  Toggle section\n"))

(defun orgist-confirm--format-value (field value)
  "Format VALUE for display based on FIELD type."
  (pcase field
    (:checked (if value "☑" "☐"))
    (:priority (format "p%s" (or value "?")))
    (:labels
     (if value
         (concat "{" (string-join (if (listp value) value (append value nil)) ", ") "}")
       "(none)"))
    ((or :due :deadline)
     (or value "(none)"))
    (:description
     (if (not value)
         "(none)"
       (let* ((flat (replace-regexp-in-string "[\n\r]+" " " value))
              (trimmed (string-trim flat)))
         (if (length> trimmed 80)
             (concat "\"" (substring trimmed 0 80) "…\"")
           (format "%S" trimmed)))))
    (:parent-id (or value "(root)"))
    (:attachment-files
     (if value
         (concat "[" (string-join value ", ") "]")
       "(none)"))
    (_
     (if value (format "%S" value) "(none)"))))

(defun orgist-confirm--count-types (items)
  "Return alist of (TYPE . COUNT) for ITEMS."
  (let ((counts '()))
    (dolist (item items)
      (let* ((diff-type (nth 2 item))
             (cell (assq diff-type counts)))
        (if cell
            (setcdr cell (1+ (cdr cell)))
          (push (cons diff-type 1) counts))))
    counts))

;;; Interaction

(defun orgist-confirm-execute ()
  "Confirm and execute the pending write-back commands."
  (interactive)
  (let ((commands orgist-confirm--commands)
        (cont orgist-confirm--continuation))
    (quit-window t)
    (orgist-execute-write-back commands)
    (when cont (funcall cont))))

(defun orgist-confirm-cancel ()
  "Cancel the write-back and close the confirmation buffer.
Does NOT call the continuation — cancelling aborts the entire
sync cycle so local changes are preserved for the next run."
  (interactive)
  (quit-window t)
  (orgist-log 'info "Write-back cancelled by user")
  ;; Clear sync mutex so the user can sync again later.
  (setq orgist-sync-mutex nil))

(defun orgist-confirm-toggle-section ()
  "Toggle expand/collapse of the node at point.
On a project line, toggles the task list.
On a task line, toggles the field details."
  (interactive)
  (let ((node (orgist-confirm--node-at-point)))
    (when node
      (let* ((ov (plist-get node :overlay))
             (marker (plist-get node :marker))
             (hidden (overlay-get ov 'invisible))
             (inhibit-read-only t))
        (overlay-put ov 'invisible (not hidden))
        ;; Update the toggle arrow on the header line
        (save-excursion
          (goto-char marker)
          (when (search-forward (if hidden "▶" "▼")
                                (line-end-position) t)
            (replace-match (if hidden "▼" "▶") t t)))))))

(defun orgist-confirm--visible-p (pos)
  "Return non-nil if POS is not inside an invisible overlay."
  (not (cl-some (lambda (ov)
                  (and (overlay-get ov 'invisible)
                       (overlay-get ov 'orgist-confirm-node)))
                (overlays-at pos))))

(defun orgist-confirm-next-section ()
  "Move point to the next visible collapsible heading."
  (interactive)
  (let ((pos (point))
        (found nil))
    (dolist (node orgist-confirm--nodes)
      (let ((m (marker-position (plist-get node :marker))))
        (when (and (> m pos)
                   (orgist-confirm--visible-p m)
                   (or (not found) (< m found)))
          (setq found m))))
    (when found (goto-char found))))

(defun orgist-confirm-prev-section ()
  "Move point to the previous visible collapsible heading."
  (interactive)
  (let ((pos (point))
        (found nil))
    (dolist (node orgist-confirm--nodes)
      (let ((m (marker-position (plist-get node :marker))))
        (when (and (< m pos)
                   (orgist-confirm--visible-p m)
                   (or (not found) (> m found)))
          (setq found m))))
    (when found (goto-char found))))

(defun orgist-confirm--node-at-point ()
  "Return the node plist for the heading at point, or nil.
Only matches if point is on the same line as the node's marker."
  (let ((line-beg (line-beginning-position))
        (line-end (line-end-position))
        (best nil)
        (best-pos -1))
    (dolist (node orgist-confirm--nodes)
      (let ((m (marker-position (plist-get node :marker))))
        (when (and (>= m line-beg) (<= m line-end) (> m best-pos))
          (setq best node
                best-pos m))))
    best))

;;; Mode definition

(defvar orgist-confirm-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (keymap-set map "C-c C-c" #'orgist-confirm-execute)
    (keymap-set map "C-c C-k" #'orgist-confirm-cancel)
    (keymap-set map "TAB" #'orgist-confirm-toggle-section)
    (keymap-set map "<tab>" #'orgist-confirm-toggle-section)
    (keymap-set map "n" #'orgist-confirm-next-section)
    (keymap-set map "p" #'orgist-confirm-prev-section)
    (keymap-set map "q" #'orgist-confirm-cancel)
    map)
  "Keymap for `orgist-confirm-mode'.")

(define-derived-mode orgist-confirm-mode special-mode "Orgist-Confirm"
  "Major mode for confirming orgist write-back changes.

\\{orgist-confirm-mode-map}"
  (setq-local revert-buffer-function #'ignore)
  (setq truncate-lines t))

(provide 'orgist-confirm)

;;; orgist-confirm.el ends here
