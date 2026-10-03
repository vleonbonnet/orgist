;;; org-sync-safety.el --- Safety net for syncing Org files with remote services -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Valentin Leon

;; Author: Valentin Leon <valentin@leon.click>
;; Keywords: outlines, tools
;; URL: https://github.com/vleonbonnet/orgist
;; Package-Requires: ((emacs "29.1") (org "9.6"))

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

;;; Commentary:

;; A sync package rewrites Org files the user also edits.  This library
;; gives it independent layers so that no sync can lose text silently:
;;
;; - History: a shadow git repository per synced directory, separate
;;   from any repository the user keeps, records the files before and
;;   after every sync cycle.  Its log answers what a given sync did,
;;   and any version can be restored.  Without git, content-addressed
;;   copies take its place.
;; - Journal: an Org file receives every piece of text a sync removes,
;;   verbatim, with what removed it and why.
;; - Trash: files a sync removes are moved aside instead of deleted.
;; - Guards: an edit confined to one entry must stay inside it, and a
;;   whole cycle may only remove what its events explain.
;;
;; Nothing here knows about a particular service; orgist uses it for
;; Todoist.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'org)

(defgroup org-sync-safety nil
  "Safety net for packages that sync Org files with remote services."
  :group 'org)

(defcustom org-sync-safety-git-program "git"
  "Git executable used for history.
When it cannot be found, history falls back to content-addressed copies."
  :group 'org-sync-safety
  :type 'string)

(defcustom org-sync-safety-copies-keep-days 14
  "Days during which every copy is kept, for histories without git."
  :group 'org-sync-safety
  :type 'integer)

(defcustom org-sync-safety-copies-daily-days 365
  "Days during which one copy per day is kept, for histories without git.
Older copies are deleted."
  :group 'org-sync-safety
  :type 'integer)

(define-error 'org-sync-safety-violation "Sync safety violation")

;;;; History

(cl-defstruct (org-sync-safety-history
               (:constructor org-sync-safety-history--create)
               (:copier nil))
  "Record of a directory's files across sync cycles."
  root       ; directory whose files are recorded
  directory  ; where the history lives
  paths      ; glob patterns, relative to ROOT, of the recorded files
  backend)   ; `git' or `copies'

(defun org-sync-safety-history (root directory paths)
  "Return a history recording the files of ROOT that match PATHS.
PATHS are glob patterns relative to ROOT, matching top-level files
only (\"*.org\", \"state.el\").  The history lives in DIRECTORY,
which may be inside ROOT; it is created on first use.  It is a git
repository when `org-sync-safety-git-program' is available, and a
tree of content-addressed copies otherwise."
  (org-sync-safety-history--create
   :root (file-name-as-directory (expand-file-name root))
   :directory (file-name-as-directory (expand-file-name directory))
   :paths paths
   :backend (if (executable-find org-sync-safety-git-program) 'git 'copies)))

(defun org-sync-safety--git (history args &optional coding)
  "Run git with ARGS on HISTORY and return its standard output.
Output is decoded with CODING (default `utf-8').  Signals an error
carrying git's standard error when git exits non-zero."
  (let ((stderr (make-temp-file "org-sync-safety-git-"))
        (root (org-sync-safety-history-root history)))
    (unwind-protect
        (with-temp-buffer
          (let* ((default-directory root)
                 (coding-system-for-read (or coding 'utf-8))
                 (coding-system-for-write 'utf-8)
                 (process-environment (append '("GIT_TERMINAL_PROMPT=0"
                                                "GIT_OPTIONAL_LOCKS=0")
                                              process-environment))
                 (status (apply #'call-process org-sync-safety-git-program
                                nil (list t stderr) nil
                                "-c" "safe.directory=*"
                                (concat "--git-dir=" (org-sync-safety-history-directory history))
                                (concat "--work-tree=" root)
                                args)))
            (unless (eql status 0)
              (error "git %s failed (%s): %s" (car args) status
                     (string-trim (with-temp-buffer
                                    (insert-file-contents stderr)
                                    (buffer-string)))))
            (buffer-string)))
      (delete-file stderr))))

(defun org-sync-safety--git-ok-p (history &rest args)
  "Run git with ARGS on HISTORY and return non-nil when it exits 0."
  (condition-case nil
      (progn (org-sync-safety--git history args) t)
    (error nil)))

(defun org-sync-safety--git-ensure (history)
  "Create HISTORY's repository when missing and keep its configuration.
The repository is isolated from the user's git setup: no signing, no
hooks, no line-ending conversion, and an identity of its own.  An
exclude file restricts it to the recorded paths."
  (let* ((dir (org-sync-safety-history-directory history))
         (exclude (expand-file-name "info/exclude" dir)))
    (unless (file-exists-p (expand-file-name "HEAD" dir))
      (make-directory dir t)
      (org-sync-safety--git history '("init" "--quiet"))
      (dolist (setting '(("core.autocrlf" "false")
                         ("core.safecrlf" "false")
                         ("core.fsmonitor" "false")
                         ("core.quotePath" "false")
                         ("commit.gpgSign" "false")
                         ("tag.gpgSign" "false")
                         ("user.name" "org-sync-safety")
                         ("user.email" "org-sync-safety@localhost")))
        (org-sync-safety--git history (cons "config" setting)))
      (let ((hooks (expand-file-name "no-hooks" dir)))
        (make-directory hooks t)
        (org-sync-safety--git history (list "config" "core.hooksPath" hooks))))
    (let ((content (concat "# Written by org-sync-safety: record only these paths.\n/*\n"
                           (mapconcat (lambda (p) (concat "!/" p))
                                      (org-sync-safety-history-paths history) "\n")
                           "\n")))
      (unless (and (file-exists-p exclude)
                   (equal content (with-temp-buffer
                                    (insert-file-contents exclude)
                                    (buffer-string))))
        (make-directory (file-name-directory exclude) t)
        (let ((coding-system-for-write 'utf-8-unix))
          (write-region content nil exclude nil 'silent))))))

(defun org-sync-safety--matching-files (history)
  "Return the files of HISTORY's root matching its paths, relative names."
  (let ((default-directory (org-sync-safety-history-root history)))
    (sort (delete-dups
           (cl-loop for pattern in (org-sync-safety-history-paths history)
                    append (seq-filter #'file-regular-p
                                       (file-expand-wildcards pattern))))
          #'string<)))

(defun org-sync-safety-history-commit (history subject &optional body)
  "Record the current content of HISTORY's files.
SUBJECT and BODY describe the change.  Returns the new revision, or
nil when nothing changed since the last record."
  (pcase (org-sync-safety-history-backend history)
    ('git
     (org-sync-safety--git-ensure history)
     (org-sync-safety--git history '("add" "--all" "--" "."))
     (unless (org-sync-safety--git-ok-p history "diff" "--cached" "--quiet")
       (org-sync-safety--git history
                             (append (list "commit" "--quiet" "--no-verify"
                                           "--allow-empty-message" "-m" subject)
                                     (when (and body (not (string-empty-p body)))
                                       (list "-m" body))))
       (string-trim (org-sync-safety--git history '("rev-parse" "HEAD")))))
    ('copies (org-sync-safety--copies-commit history subject body))))

(defun org-sync-safety-history-log (history &optional file limit)
  "Return HISTORY's records, newest first, as (REVISION TIME SUBJECT).
TIME is a Lisp timestamp.  With FILE (relative to the root), only
records that changed it.  LIMIT caps the number of entries."
  (pcase (org-sync-safety-history-backend history)
    ('git
     (when (file-exists-p (expand-file-name "HEAD" (org-sync-safety-history-directory history)))
       (condition-case nil
           (mapcar (lambda (line)
                     (pcase-let ((`(,rev ,time ,subject) (split-string line "\t")))
                       (list rev (seconds-to-time (string-to-number time)) (or subject ""))))
                   (split-string
                    (org-sync-safety--git
                     history (append (list "log" "--format=%H%x09%ct%x09%s")
                                     (when limit (list "-n" (number-to-string limit)))
                                     (when file (list "--" file))))
                    "\n" t))
         ;; No commit yet.
         (error nil))))
    ('copies (org-sync-safety--copies-log history file limit))))

(defun org-sync-safety-history-file-at (history revision file)
  "Return the bytes of FILE at REVISION of HISTORY, or nil if absent.
The result is a unibyte string; decode it to display it."
  (pcase (org-sync-safety-history-backend history)
    ('git
     (condition-case nil
         (encode-coding-string ; keep the exact bytes
          (org-sync-safety--git history (list "show" (concat revision ":" file))
                                'no-conversion)
          'no-conversion)
       (error nil)))
    ('copies (org-sync-safety--copies-file-at history revision file))))

(defun org-sync-safety-history-restore (history revision file)
  "Write FILE as it was at REVISION of HISTORY.
The current state is recorded first, so the restore itself can be
undone.  Buffers visiting FILE are not touched; revert them."
  (let ((bytes (org-sync-safety-history-file-at history revision file)))
    (unless bytes
      (error "%s does not exist at %s" file revision))
    (org-sync-safety-history-commit history (format "Before restoring %s" file))
    (let ((coding-system-for-write 'no-conversion))
      (write-region bytes nil (expand-file-name file (org-sync-safety-history-root history))
                    nil 'silent))
    (org-sync-safety-history-commit history (format "Restored %s from %s" file
                                                    (substring revision 0 (min 12 (length revision)))))))

;;;;; Copies backend

(defun org-sync-safety--stamp-time (stamp)
  "Return the local time a copy STAMP (YYYYMMDDTHHMMSS) stands for."
  (encode-time (list (string-to-number (substring stamp 13 15))
                     (string-to-number (substring stamp 11 13))
                     (string-to-number (substring stamp 9 11))
                     (string-to-number (substring stamp 6 8))
                     (string-to-number (substring stamp 4 6))
                     (string-to-number (substring stamp 0 4))
                     nil -1 nil)))

(defun org-sync-safety--copies-dir (history)
  (expand-file-name "copies" (org-sync-safety-history-directory history)))

(defun org-sync-safety--file-sha1 (file)
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally file)
    (secure-hash 'sha1 (current-buffer))))

(defun org-sync-safety--copies-of (history file)
  "Return FILE's copies in HISTORY, oldest first, as absolute names."
  (let ((dir (expand-file-name file (org-sync-safety--copies-dir history))))
    (when (file-directory-p dir)
      (directory-files dir t "\\`[0-9]\\{8\\}T[0-9]\\{6\\}-[0-9a-f]+\\'"))))

(defun org-sync-safety--copies-commit (history subject body)
  "Copy every changed file of HISTORY; return the record ID or nil."
  (let* ((stamp (format-time-string "%Y%m%dT%H%M%S"))
         (root (org-sync-safety-history-root history))
         (copied nil))
    (dolist (file (org-sync-safety--matching-files history))
      (let* ((sha (org-sync-safety--file-sha1 (expand-file-name file root)))
             (latest (car (last (org-sync-safety--copies-of history file)))))
        (unless (and latest (string-suffix-p (concat "-" sha) latest))
          (let ((target (expand-file-name (format "%s/%s-%s" file stamp sha)
                                          (org-sync-safety--copies-dir history))))
            (make-directory (file-name-directory target) t)
            (copy-file (expand-file-name file root) target t)
            (push file copied)))))
    (when copied
      (let ((coding-system-for-write 'utf-8-unix)
            (log (expand-file-name "log.txt" (org-sync-safety--copies-dir history))))
        (write-region (format "%s\t%s\t%s\t%s\n" stamp
                              (string-join (nreverse copied) ",")
                              (replace-regexp-in-string "[\t\n]" " " subject)
                              (replace-regexp-in-string "[\t\n]" " " (or body "")))
                      nil log t 'silent))
      (org-sync-safety--copies-prune history)
      stamp)))

(defun org-sync-safety--copies-log (history file limit)
  (let ((log (expand-file-name "log.txt" (org-sync-safety--copies-dir history)))
        (entries nil))
    (when (file-exists-p log)
      (with-temp-buffer
        (insert-file-contents log)
        (dolist (line (split-string (buffer-string) "\n" t))
          (pcase-let ((`(,stamp ,files ,subject . ,_) (split-string line "\t")))
            (when (or (null file) (member file (split-string files "," t)))
              (push (list stamp (org-sync-safety--stamp-time stamp) subject) entries))))))
    (if limit (seq-take entries limit) entries)))

(defun org-sync-safety--copies-file-at (history revision file)
  "Return FILE's bytes as of record REVISION: its latest copy at or before it."
  (when-let* ((copy (car (last (seq-filter
                                (lambda (c)
                                  (not (string< revision (substring (file-name-nondirectory c) 0 15))))
                                (org-sync-safety--copies-of history file))))))
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (insert-file-contents-literally copy)
      (buffer-string))))

(defun org-sync-safety--copies-prune (history)
  "Apply the retention policy to HISTORY's copies.
Every copy younger than `org-sync-safety-copies-keep-days' stays;
then the newest copy of each day stays until
`org-sync-safety-copies-daily-days'; older copies are deleted.  A
file's newest copy is always kept."
  (let ((now (float-time))
        (day 86400.0))
    (dolist (dir (directory-files-recursively (org-sync-safety--copies-dir history) "" t))
      (when (file-directory-p dir)
        (let* ((copies (org-sync-safety--copies-of-dir dir))
               (newest (car (last copies)))
               (seen-days (make-hash-table :test 'equal)))
          (dolist (copy (reverse copies))
            (let* ((stamp (substring (file-name-nondirectory copy) 0 15))
                   (age (/ (- now (float-time (org-sync-safety--stamp-time stamp))) day))
                   (date (substring stamp 0 8)))
              (cond
               ((equal copy newest) (puthash date t seen-days))
               ((< age org-sync-safety-copies-keep-days) nil)
               ((and (< age org-sync-safety-copies-daily-days)
                     (not (gethash date seen-days)))
                (puthash date t seen-days))
               (t (delete-file copy))))))))))

(defun org-sync-safety--copies-of-dir (dir)
  (directory-files dir t "\\`[0-9]\\{8\\}T[0-9]\\{6\\}-[0-9a-f]+\\'"))

;;;; Journal

(defun org-sync-safety-journal-record (journal &rest entry)
  "Append ENTRY to the JOURNAL file, an Org file created on demand.
ENTRY is a plist: :title (heading text), :text (the removed text,
kept verbatim), and optionally :kind, :file, :outline (a list of
heading titles), :id, :reason and :cycle, stored as properties
(:file as SOURCE, since FILE is an Org special property)."
  (let* ((text (or (plist-get entry :text) ""))
         (props (cl-loop for (key . name) in '((:kind . "KIND") (:file . "SOURCE")
                                              (:outline . "OUTLINE") (:id . "ELEMENT")
                                              (:reason . "REASON") (:cycle . "CYCLE"))
                         for value = (plist-get entry key)
                         when value
                         collect (cons name (org-sync-safety--one-line
                                             (if (listp value)
                                                 (string-join value " / ")
                                               (format "%s" value))))))
         (record (concat
                  (format "* %s %s\n"
                          (format-time-string (org-time-stamp-format t t))
                          (org-sync-safety--one-line (or (plist-get entry :title) "Removed text")))
                  ":PROPERTIES:\n"
                  (mapconcat (lambda (p) (format ":%s: %s\n" (car p) (cdr p))) props "")
                  ":END:\n"
                  "#+begin_src org\n"
                  (org-escape-code-in-string
                   (if (string-suffix-p "\n" text) text (concat text "\n")))
                  "#+end_src\n\n")))
    (make-directory (file-name-directory (expand-file-name journal)) t)
    (let ((coding-system-for-write 'utf-8-unix))
      (unless (file-exists-p journal)
        (write-region "#+TITLE: Sync journal\n#+STARTUP: overview\n\nText removed by syncs, newest last.\n\n"
                      nil journal nil 'silent))
      (write-region record nil journal t 'silent))))

(defun org-sync-safety--one-line (string)
  "Return STRING with line breaks collapsed, for a property value."
  (string-trim (replace-regexp-in-string "[\n\r]+" " " string)))

(defun org-sync-safety-journal-entry ()
  "Return the journal entry at point as a plist, or nil.
Keys: :title, :text (unescaped), :kind, :file, :outline, :id,
:reason, :cycle."
  (save-excursion
    (when (ignore-errors (org-back-to-heading t) t)
      (while (> (org-current-level) 1) (org-up-heading-safe))
      (let ((end (save-excursion (org-end-of-subtree t t) (point)))
            text)
        (save-excursion
          (when (re-search-forward "^#\\+begin_src org\n" end t)
            (let ((beg (point)))
              (when (re-search-forward "^#\\+end_src$" end t)
                (setq text (org-unescape-code-in-string
                            (buffer-substring-no-properties beg (match-beginning 0))))))))
        (list :title (org-get-heading t t t t)
              :text text
              :kind (org-entry-get (point) "KIND")
              :file (org-entry-get (point) "SOURCE")
              :outline (org-entry-get (point) "OUTLINE")
              :id (org-entry-get (point) "ELEMENT")
              :reason (org-entry-get (point) "REASON")
              :cycle (org-entry-get (point) "CYCLE"))))))

;;;; Trash

(defun org-sync-safety-trash-file (file trash &optional relative-to)
  "Move FILE into directory TRASH instead of deleting it; return its new name.
The file lands under a directory named after today's date, below its
path relative to RELATIVE-TO when it lies inside it; an existing
name gets a numeric suffix."
  (let* ((relative (if (and relative-to (file-in-directory-p file relative-to))
                       (file-relative-name file relative-to)
                     (file-name-nondirectory file)))
         (target (expand-file-name relative
                                   (expand-file-name (format-time-string "%Y-%m-%d")
                                                     trash)))
         (base target)
         (n 1))
    (while (file-exists-p target)
      (setq n (1+ n)
            target (format "%s.%d" base n)))
    (make-directory (file-name-directory target) t)
    (rename-file file target)
    target))

;;;; Guards

(defvar org-sync-safety-guard-suspended nil
  "Non-nil while changes are exempt from region guards.
Bound around user hook functions by
`org-sync-safety-with-unguarded-hooks'.")

(defun org-sync-safety-call-with-region-guard (beg end what fn)
  "Call FN, which may only change the current buffer between BEG and END.
Changes are recorded as FN makes them; END follows insertions at it.
When one falls outside, signal `org-sync-safety-violation' with WHAT
and the offending regions, after FN returns.  Changes made while
`org-sync-safety-guard-suspended' is non-nil are not checked."
  (let* ((start (copy-marker beg nil))
         (stop (copy-marker end t))
         (outside nil)
         (recorder (lambda (b e _len)
                     (when (and (not org-sync-safety-guard-suspended)
                                (or (< b start) (> e stop)))
                       (push (cons b e) outside)))))
    (add-hook 'after-change-functions recorder nil t)
    (unwind-protect
        (prog1 (funcall fn)
          (when outside
            (signal 'org-sync-safety-violation
                    (list (format "%s changed text outside its region %d-%d: %S"
                                  what (marker-position start) (marker-position stop)
                                  (seq-take (nreverse outside) 5))))))
      (remove-hook 'after-change-functions recorder t)
      (set-marker start nil)
      (set-marker stop nil))))

(defmacro org-sync-safety-with-region-guard (beg end what &rest body)
  "Run BODY, which may only change the current buffer between BEG and END.
See `org-sync-safety-call-with-region-guard'."
  (declare (indent 3))
  `(org-sync-safety-call-with-region-guard ,beg ,end ,what (lambda () ,@body)))

(defun org-sync-safety--unguarded-hook-value (hook)
  "Return the functions HOOK runs, each running with guards suspended.
A buffer-local value's t element is replaced by the global functions,
as `run-hooks' would."
  (let ((value (symbol-value hook)))
    (mapcar (lambda (fn)
              (lambda (&rest args)
                (let ((org-sync-safety-guard-suspended t))
                  (apply fn args))))
            (cl-loop for fn in (if (functionp value) (list value) value)
                     if (eq fn t)
                     append (let ((global (default-value hook)))
                              (if (functionp global) (list global) global))
                     else collect fn))))

(defmacro org-sync-safety-with-unguarded-hooks (hooks &rest body)
  "Run BODY with the functions of HOOKS unguarded.
HOOKS is evaluated to a list of hook symbols.  User hooks run by the
code a guard watches, such as a dependency trigger completing the
next task, may legitimately change other entries; their changes are
exempt while the sync's own are checked."
  (declare (indent 1))
  (let ((bound (make-symbol "bound")))
    `(let ((,bound (seq-filter #'boundp ,hooks)))
       (cl-progv ,bound (mapcar #'org-sync-safety--unguarded-hook-value ,bound)
         ,@body))))

(defun org-sync-safety-compare-ids (before after allowed)
  "Return the problems a sync cycle caused to element IDs.
BEFORE and AFTER are hash tables mapping each ID to the list of
files holding it, before and after the cycle.  ALLOWED is a hash
table of IDs the cycle was entitled to remove.  A problem is an ID
that disappeared without being allowed, or one held more than once
after the cycle.  Returns a list of strings, nil when consistent."
  (let ((problems nil))
    (maphash (lambda (id files)
               (unless (or (gethash id after) (gethash id allowed))
                 (push (format "%s vanished from %s" id (string-join files ", "))
                       problems)))
             before)
    (maphash (lambda (id files)
               (when (cdr files)
                 (push (format "%s is held %d times (%s)" id (length files)
                               (string-join files ", "))
                       problems)))
             after)
    (sort problems #'string<)))

(defun org-sync-safety-shrunk-files (before after removed &optional ratio minimum)
  "Return the files a sync cycle shrank suspiciously.
BEFORE and AFTER map file names to sizes.  A file that lost more
than RATIO (default 0.5) of its size, and was at least MINIMUM
\(default 2000) bytes, is suspicious unless REMOVED, a list of
files the cycle was entitled to delete, includes it.  Returns a
list of strings."
  (let ((ratio (or ratio 0.5))
        (minimum (or minimum 2000))
        (problems nil))
    (maphash (lambda (file size)
               (let ((now (gethash file after)))
                 (when (and (>= size minimum)
                            (not (member file removed))
                            (< (or now 0) (* size (- 1 ratio))))
                   (push (format "%s shrank from %d to %d bytes" file size (or now 0))
                         problems))))
             before)
    (sort problems #'string<)))

(provide 'org-sync-safety)
;;; org-sync-safety.el ends here
