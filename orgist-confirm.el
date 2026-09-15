;;; orgist-confirm.el --- Write-back review via org-sync-confirm -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2026 Valentin Leon

;; This file is part of orgist.

;;; Commentary:

;; Turns the local changes detected by `orgist-diff-all-elements' into
;; an `org-sync-confirm' tree (project > task > field), fetches the
;; live Todoist state of modified tasks so the old side of every diff
;; is what the write will overwrite, and executes the confirmed
;; subset through `orgist-execute-write-back'.
;;
;; When the user leaves some changes unticked, the commands are
;; regenerated for the ticked changes only and the verification stamps
;; are dropped, so the files stay due and the unticked changes are
;; offered again on the next save or sync.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'org-sync-confirm)

;; Forward declarations for orgist.el (avoids a circular require).
(declare-function orgist-execute-write-back "orgist" (commands))
(declare-function orgist-changes-to-commands "orgist" (changes))
(declare-function orgist-log "orgist" (level format-string &rest args))
(declare-function orgist-find-element-by-id "orgist" (element-id))
(declare-function orgist-extract-heading-and-tags "orgist" ())
(declare-function orgist-element-local-state "orgist" ())
(declare-function orgist-extract-logbook-notes "orgist" ())
(declare-function orgist-convert-content "orgist" (content))
(declare-function orgist-convert-description "orgist" (description level))
(declare-function orgist-parse-todoist-date-with-duration "orgist" (date-info duration-info))
(declare-function orgist--label-to-tag "orgist" (label-name))
(declare-function orgist--request-with-retry "orgist" (url &rest args))
(declare-function org-current-level "org" ())
(defvar orgist-snapshots)
(defvar orgist-sync-mutex)
(defvar orgist-base-dir)
(defvar orgist-bearer-token)
(defvar orgist--pending-stamps)

(defcustom orgist-confirm-remote-fetch-limit 20
  "Fetch the live Todoist state of at most this many modified elements.
The review buffer then shows the remote value as the old side of
each diff and warns when it no longer matches the last sync.  Above
the limit, or when a fetch fails, the snapshot from the last sync is
used instead.  Set to 0 to never fetch."
  :group 'orgist
  :type 'integer)

;;; Element lookup

(defun orgist-confirm--project-buffers ()
  "Return an alist of (PROJECT-NAME . BUFFER) for the open project files."
  (let ((out nil))
    (dolist (file (directory-files orgist-base-dir t "\\`[^.].*\\.org\\'"))
      (when-let* ((buf (find-buffer-visiting file)))
        (push (cons (file-name-base file) buf) out)))
    (nreverse out)))

(defun orgist-confirm--locate (id buffers)
  "Return (PROJECT-NAME BUFFER . POS) for element ID among BUFFERS, or nil."
  (catch 'found
    (dolist (entry buffers)
      (with-current-buffer (cdr entry)
        (when-let* ((pos (orgist-find-element-by-id id)))
          (throw 'found (cons (car entry) (cons (cdr entry) pos))))))
    nil))

(defun orgist-confirm--project-of-deleted (snapshot buffers)
  "Return the project name of a deleted element from its SNAPSHOT.
Walks :parent-id up to a file-level ID among BUFFERS."
  (let ((parent (plist-get snapshot :parent-id))
        (seen nil)
        (name nil))
    (while (and parent (not name) (not (member parent seen)))
      (push parent seen)
      (let ((loc (orgist-confirm--locate parent buffers)))
        (cond
         ((null loc) (setq parent nil))
         ((= (cddr loc) (with-current-buffer (cadr loc) (point-min)))
          (setq name (car loc)))
         (t (setq parent (plist-get (gethash parent orgist-snapshots) :parent-id))))))
    name))

;;; Value formatting

(defun orgist-confirm--format-value (field value)
  "Format VALUE of FIELD for display."
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
     (if (and value (not (string-empty-p (string-trim value))))
         (string-trim value)
       "(none)"))
    (:parent-id
     (if value
         (or (plist-get (gethash value orgist-snapshots) :content) value)
       "(root)"))
    (:attachment-files
     (if value
         (concat "[" (string-join value ", ") "]")
       "(none)"))
    (_
     (if value (format "%S" value) "(none)"))))

(defun orgist-confirm--field-value (field value)
  "Return the display string for VALUE of FIELD, or nil when absent."
  (cond
   ((eq field :checked) (orgist-confirm--format-value field value))
   ((null value) nil)
   ((and (eq field :description) (string-empty-p (string-trim value))) nil)
   (t (orgist-confirm--format-value field value))))

(defun orgist-confirm--field-name (field)
  "Return the display name of FIELD."
  (if (eq field :content) "title" (substring (symbol-name field) 1)))

;;; Live remote state

(defun orgist-confirm--token ()
  "Return the bearer token, calling it when it is a function."
  (if (functionp orgist-bearer-token)
      (funcall orgist-bearer-token)
    orgist-bearer-token))

(defun orgist-confirm--fetch-remote (id section-p)
  "Return the current Todoist state of element ID as an alist, or nil."
  (let ((result nil))
    (orgist--request-with-retry
     (format "https://api.todoist.com/api/v1/%s/%s" (if section-p "sections" "tasks") id)
     :headers `(("Authorization" . ,(format "Bearer %s" (orgist-confirm--token))))
     :parser 'json-read
     :sync t
     :error (cl-function
             (lambda (&key error-thrown &allow-other-keys)
               (orgist-log 'warn "Confirm: could not fetch %s: %S" id error-thrown)))
     :success (cl-function
               (lambda (&key data &allow-other-keys)
                 (setq result data))))
    result))

(defun orgist-confirm--remote-state (remote section-p level)
  "Convert the REMOTE alist into the plist shape of a snapshot.
SECTION-P selects the section field names; LEVEL is the org level
used to convert the description."
  (let ((description (alist-get 'description remote)))
    (list :content (orgist-convert-content (alist-get (if section-p 'name 'content) remote))
          :checked (eq (alist-get 'checked remote) t)
          :priority (alist-get 'priority remote)
          :labels (mapcar #'orgist--label-to-tag (append (alist-get 'labels remote) nil))
          :due (orgist-parse-todoist-date-with-duration
                (alist-get 'due remote) (alist-get 'duration remote))
          :deadline (orgist-parse-todoist-date-with-duration (alist-get 'deadline remote) nil)
          :description (when (and description (not (string-empty-p description)))
                         (orgist-convert-description description level)))))

(defconst orgist-confirm--remote-fields
  '(:content :checked :priority :labels :due :deadline :description)
  "Snapshot fields that the live remote state can replace.")

(defun orgist-confirm--same-value-p (a b)
  "Return non-nil when A and B are equal after whitespace normalisation."
  (cl-flet ((norm (v)
              (if (stringp v)
                  (string-trim (replace-regexp-in-string "[ \t\n\r]+" " " v))
                v)))
    (equal (norm a) (norm b))))

;;; Items

(defun orgist-confirm--payload (commands)
  "Return COMMANDS pretty-printed as JSON, or nil when there are none."
  (when commands
    (let ((json-encoding-pretty-print t)
          (json-encoding-default-indentation "  "))
      (mapconcat #'json-encode commands "\n"))))

(defun orgist-confirm--command-element-id (cmd)
  "Return the element ID that command CMD acts on, or nil."
  (let ((args (alist-get 'args cmd)))
    (or (alist-get 'id args)
        (alist-get 'item_id args)
        (when-let* ((vec (or (alist-get 'items args) (alist-get 'sections args))))
          (alist-get 'id (aref vec 0)))
        (when (member (alist-get 'type cmd) '("item_add" "section_add"))
          (alist-get 'temp_id cmd)))))

(defun orgist-confirm--commands-by-element (commands)
  "Return (TABLE . REST): COMMANDS keyed by element ID, and the unkeyed rest."
  (let ((table (make-hash-table :test 'equal))
        (rest nil))
    (dolist (cmd commands)
      (if-let* ((id (orgist-confirm--command-element-id cmd)))
          (puthash id (append (gethash id table) (list cmd)) table)
        (push cmd rest)))
    (cons table (nreverse rest))))

(defun orgist-confirm--new-fields (local)
  "Return the fields describing a new element from its LOCAL state."
  (let ((fields nil))
    (cl-flet ((add (name value)
                (when value (push (list :name name :new value) fields))))
      (add "title" (plist-get local :content))
      (when (and (plist-get local :priority) (> (plist-get local :priority) 1))
        (add "priority" (orgist-confirm--format-value :priority (plist-get local :priority))))
      (when (plist-get local :labels)
        (add "labels" (orgist-confirm--format-value :labels (plist-get local :labels))))
      (add "due" (plist-get local :due))
      (add "deadline" (plist-get local :deadline))
      (add "duration" (when-let* ((d (plist-get local :duration))) (format "%S" d)))
      (add "description" (orgist-confirm--field-value :description (plist-get local :description)))
      (when (plist-get local :attachment-files)
        (add "attachment-files" (orgist-confirm--format-value
                                 :attachment-files (plist-get local :attachment-files)))))
    (nreverse fields)))

(defun orgist-confirm--new-notes (pos known-count)
  "Return the note texts at POS beyond the first KNOWN-COUNT ones."
  (save-excursion
    (goto-char pos)
    (mapcar #'cdr (nthcdr known-count (seq-uniq (orgist-extract-logbook-notes) #'equal)))))

(defun orgist-confirm--modified-fields (diff snapshot remote loc)
  "Return (FIELDS . REMOTE-CHANGED) for a modified element.
DIFF is the alist from `orgist-diff-element', SNAPSHOT its last-sync
state, REMOTE its live state or nil, LOC its (PROJECT BUFFER . POS)."
  (let ((fields nil)
        (remote-changed nil))
    (dolist (change diff)
      (let* ((field (car change))
             (old (cadr change))
             (new (cddr change)))
        (cond
         ((eq field :notes)
          (dolist (note (if loc (orgist-confirm--new-notes (cddr loc) old) nil))
            (push (list :name "note" :new note) fields)))
         (t
          (when (and remote (memq field orgist-confirm--remote-fields))
            (let ((live (plist-get remote field)))
              (unless (orgist-confirm--same-value-p live (plist-get snapshot field))
                (push (orgist-confirm--field-name field) remote-changed))
              (setq old live)))
          (push (list :name (orgist-confirm--field-name field)
                      :old (orgist-confirm--field-value field old)
                      :new (orgist-confirm--field-value field new))
                fields)))))
    (cons (nreverse fields) (nreverse remote-changed))))

(defun orgist-confirm--item (change buffers remotes commands)
  "Return (PROJECT . ITEM) for CHANGE.
BUFFERS is the project buffer alist, REMOTES the live-state table,
COMMANDS the per-element command table."
  (let* ((id (car change))
         (diff (cdr change))
         (snapshot (gethash id orgist-snapshots))
         (loc (orgist-confirm--locate id buffers))
         (heading (when loc
                    (with-current-buffer (cadr loc)
                      (save-excursion
                        (goto-char (cddr loc))
                        (car (orgist-extract-heading-and-tags))))))
         (label (or heading (plist-get snapshot :content) "?"))
         (project (or (car loc)
                      (and snapshot (orgist-confirm--project-of-deleted snapshot buffers))
                      "?"))
         (payload (orgist-confirm--payload (gethash id commands)))
         (item
          (pcase diff
            ('deleted
             (list :label label :kind 'deleted :warning "permanent"
                   :fields (delq nil
                                 (list (when-let* ((d (orgist-confirm--field-value
                                                       :description
                                                       (plist-get snapshot :description))))
                                         (list :name "description" :old d))))))
            ((or 'new 'new-section)
             (let ((local (when loc
                            (with-current-buffer (cadr loc)
                              (save-excursion
                                (goto-char (cddr loc))
                                (orgist-element-local-state))))))
               (list :label label :kind 'new
                     :summary (if (eq diff 'new-section) "new section" "new")
                     :fields (when local (orgist-confirm--new-fields local)))))
            (_
             (let ((parts (orgist-confirm--modified-fields
                           diff snapshot (gethash id remotes) loc)))
               (list :label label :kind 'modified
                     :fields (car parts)
                     :warning (when (cdr parts)
                                (format "changed in Todoist since last sync: %s"
                                        (string-join (cdr parts) ", ")))))))))
    (cons project (append item (list :payload payload :data change)))))

(defun orgist-confirm--fetch-remotes (changes buffers)
  "Return a table ID → live state for the modified elements of CHANGES."
  (let ((table (make-hash-table :test 'equal))
        (modified (seq-filter (lambda (c) (consp (cdr c))) changes)))
    (if (> (length modified) orgist-confirm-remote-fetch-limit)
        (orgist-log 'info "Confirm: %d modified elements exceed the remote fetch limit (%d), using snapshots"
                    (length modified) orgist-confirm-remote-fetch-limit)
      (dolist (change modified)
        (let* ((id (car change))
               (snapshot (gethash id orgist-snapshots))
               (section-p (plist-get snapshot :section-p))
               (loc (orgist-confirm--locate id buffers))
               (level (or (when loc
                            (with-current-buffer (cadr loc)
                              (save-excursion (goto-char (cddr loc)) (org-current-level))))
                          1))
               (remote (orgist-confirm--fetch-remote id section-p)))
          (when remote
            (puthash id (orgist-confirm--remote-state remote section-p level) table)))))
    table))

(defun orgist-confirm--items (changes commands)
  "Build the `org-sync-confirm' tree for CHANGES and their COMMANDS."
  (let* ((buffers (orgist-confirm--project-buffers))
         (remotes (orgist-confirm--fetch-remotes changes buffers))
         (by-element (orgist-confirm--commands-by-element commands))
         (projects nil))
    (dolist (change changes)
      (let* ((entry (orgist-confirm--item change buffers remotes (car by-element)))
             (cell (or (assoc (car entry) projects)
                       (let ((c (list (car entry))))
                         (push c projects)
                         c))))
        (setcdr cell (append (cdr cell) (list (cdr entry))))))
    (setq projects (sort projects (lambda (a b) (string< (car a) (car b)))))
    (append
     (mapcar (lambda (p) (list :label (car p) :children (cdr p))) projects)
     (when (cdr by-element)
       (list (list :label "Batch"
                   :children
                   (mapcar (lambda (cmd)
                             (list :label (format "%s %s"
                                                  (alist-get 'type cmd)
                                                  (or (alist-get 'name (alist-get 'args cmd)) ""))
                                   :kind 'new :fixed t
                                   :summary "sent with the batch"
                                   :payload (orgist-confirm--payload (list cmd))))
                           (cdr by-element))))))))

;;; Execution

(defun orgist-confirm--execute (items total commands)
  "Send the changes carried by ITEMS.
TOTAL is the number of changes offered; COMMANDS the batch built for
all of them.  A strict subset is regenerated for the ticked changes
only and the verification stamps are dropped so the rest re-detects."
  (let ((changes (delq nil (mapcar (lambda (i) (plist-get i :data)) items))))
    (if (= (length changes) total)
        (orgist-execute-write-back commands)
      (orgist-log 'info "Write-back: %d of %d change(s) selected, the rest stays pending"
                  (length changes) total)
      (setq orgist--pending-stamps nil)
      (orgist-execute-write-back (orgist-changes-to-commands changes)))))

(defun orgist-confirm--cancel ()
  "Abort the sync cycle so local changes are offered again later."
  (orgist-log 'info "Write-back cancelled by user")
  (setq orgist-sync-mutex nil))

;;; Entry point

(defun orgist-confirm-show (changes commands &optional continuation)
  "Display the review buffer for write-back CHANGES and their COMMANDS.
CHANGES is the list of (ID . DIFF) pairs from `orgist-diff-all-elements'.
CONTINUATION, if non-nil, is called after a confirmed write-back to
continue the sync flow.  It is NOT called on cancel."
  (let ((total (length changes)))
    (org-sync-confirm-show
     (list :title "Orgist Write-Back"
           :buffer "*orgist-confirm*"
           :confirm-label "Confirm"
           :items (orgist-confirm--items changes commands)
           :execute (lambda (items) (orgist-confirm--execute items total commands))
           :on-success (lambda (_items) (when continuation (funcall continuation)))
           :on-cancel #'orgist-confirm--cancel))))

(provide 'orgist-confirm)

;;; orgist-confirm.el ends here
