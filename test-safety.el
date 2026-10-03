;;; test-safety.el --- Tests for org-sync-safety -*- lexical-binding: t; -*-

;; Usage: emacs --batch -L . -l ert -l test-safety.el -f ert-run-tests-batch-and-exit
;;
;; The safety net a sync package relies on before it rewrites files
;; the user also edits: history (git and copies backends), journal,
;; trash, and the guards that abort a sync whose effect its events do
;; not explain.

(add-to-list 'load-path default-directory)
(require 'test-isolation)
(require 'ert)
(require 'cl-lib)
(require 'org-sync-safety)

(defconst org-sync-safety-test--unicode "* TODO Café — résumé ✓ 🌱\nNotes « déjà ».\n"
  "Content whose bytes must survive a record and restore unchanged.")

(defmacro org-sync-safety-test--with-root (&rest body)
  "Run BODY with ROOT bound to a fresh directory holding sample files."
  (declare (indent 0))
  `(let ((root (file-name-as-directory (make-temp-file "org-sync-safety-" t))))
     (unwind-protect
         (progn
           (org-sync-safety-test--write root "A.org" org-sync-safety-test--unicode)
           (org-sync-safety-test--write root "B.org" "* TODO B\n")
           (org-sync-safety-test--write root "state.el" "(state)\n")
           (org-sync-safety-test--write root "notes.txt" "not recorded\n")
           (make-directory (expand-file-name "data" root) t)
           (org-sync-safety-test--write root "data/blob.org" "not recorded either\n")
           ,@body)
       (delete-directory root t))))

(defun org-sync-safety-test--write (root file content)
  (let ((coding-system-for-write 'utf-8-unix))
    (write-region content nil (expand-file-name file root) nil 'silent)))

(defun org-sync-safety-test--read-bytes (root file)
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally (expand-file-name file root))
    (buffer-string)))

(defun org-sync-safety-test--history (root backend)
  (let ((history (org-sync-safety-history root (expand-file-name ".history" root)
                                          '("*.org" "state.el"))))
    (setf (org-sync-safety-history-backend history) backend)
    history))

(defun org-sync-safety-test--lifecycle (backend)
  "Exercise record, log, file-at and restore on BACKEND."
  (org-sync-safety-test--with-root
    (let* ((history (org-sync-safety-test--history root backend))
           (first (org-sync-safety-history-commit history "First" "Body line")))
      (should first)
      ;; Nothing changed: no new record.
      (should-not (org-sync-safety-history-commit history "Again"))
      (when (eq backend 'copies) (sleep-for 1.1)) ; distinct stamps
      (org-sync-safety-test--write root "A.org" "* TODO Changed\n")
      (let ((second (org-sync-safety-history-commit history "Second")))
        (should second)
        (should (equal (mapcar #'caddr (org-sync-safety-history-log history "A.org"))
                       '("Second" "First")))
        ;; B.org only changed in the first record.
        (should (equal (mapcar #'caddr (org-sync-safety-history-log history "B.org"))
                       '("First")))
        ;; Unrecorded files never enter the history.
        (should-not (org-sync-safety-history-file-at history second "notes.txt"))
        (should-not (org-sync-safety-history-file-at history second "data/blob.org"))
        ;; Exact bytes come back.
        (should (equal (org-sync-safety-history-file-at history first "A.org")
                       (encode-coding-string org-sync-safety-test--unicode 'utf-8)))
        (when (eq backend 'copies) (sleep-for 1.1))
        (org-sync-safety-history-restore history first "A.org")
        (should (equal (org-sync-safety-test--read-bytes root "A.org")
                       (encode-coding-string org-sync-safety-test--unicode 'utf-8)))
        (should (equal (caddr (car (org-sync-safety-history-log history "A.org")))
                       (format "Restored A.org from %s"
                               (substring first 0 (min 12 (length first))))))))))

(ert-deftest org-sync-safety/git-history-lifecycle ()
  "The git backend records, lists, reads back and restores exact bytes."
  (skip-unless (executable-find org-sync-safety-git-program))
  (org-sync-safety-test--lifecycle 'git))

(ert-deftest org-sync-safety/copies-history-lifecycle ()
  "The copies backend does the same without git."
  (org-sync-safety-test--lifecycle 'copies))

(ert-deftest org-sync-safety/git-history-records-deletions ()
  "A deleted file is recorded as such and can be restored."
  (skip-unless (executable-find org-sync-safety-git-program))
  (org-sync-safety-test--with-root
    (let* ((history (org-sync-safety-test--history root 'git))
           (first (org-sync-safety-history-commit history "First")))
      (delete-file (expand-file-name "B.org" root))
      (let ((second (org-sync-safety-history-commit history "Deleted B")))
        (should second)
        (should-not (org-sync-safety-history-file-at history second "B.org"))
        (org-sync-safety-history-restore history first "B.org")
        (should (file-exists-p (expand-file-name "B.org" root)))))))

(ert-deftest org-sync-safety/git-history-is-isolated ()
  "The shadow repository ignores the user's signing and hook settings."
  (skip-unless (executable-find org-sync-safety-git-program))
  (org-sync-safety-test--with-root
    (let ((history (org-sync-safety-test--history root 'git)))
      (org-sync-safety-history-commit history "First")
      (should (equal (string-trim (org-sync-safety--git history '("config" "commit.gpgSign")))
                     "false"))
      (should (string-suffix-p "no-hooks"
                               (string-trim (org-sync-safety--git history '("config" "core.hooksPath")))))
      (should (equal (split-string (org-sync-safety--git history '("ls-files")) "\n" t)
                     '("A.org" "B.org" "state.el"))))))

(ert-deftest org-sync-safety/copies-retention ()
  "Old copies thin out to one a day, then disappear; the newest always stays."
  (org-sync-safety-test--with-root
    (let* ((history (org-sync-safety-test--history root 'copies))
           (dir (expand-file-name "copies/A.org" (expand-file-name ".history" root)))
           (stamp (lambda (days-ago hour)
                    (format-time-string "%Y%m%dT%H0000"
                                        (time-subtract nil (+ (* days-ago 86400) (* hour 3600)))))))
      (make-directory dir t)
      (dolist (spec '((400 0) (100 1) (100 2) (3 1) (3 2)))
        (write-region "x" nil (expand-file-name
                               (concat (funcall stamp (car spec) (cadr spec)) "-aaaa")
                               dir)
                      nil 'silent))
      (org-sync-safety--copies-prune history)
      (let ((left (mapcar (lambda (f) (substring (file-name-nondirectory f) 0 15))
                          (org-sync-safety--copies-of-dir dir))))
        ;; 400 days old: gone.  100 days old: one of two kept.  3 days: both.
        (should (= (length left) 3))
        (should-not (member (funcall stamp 400 0) left))
        (should (member (funcall stamp 3 1) left))
        (should (member (funcall stamp 3 2) left))))))

(ert-deftest org-sync-safety/journal-round-trip ()
  "Removed text is journaled verbatim, Org syntax included, and read back."
  (let* ((dir (make-temp-file "org-sync-safety-journal-" t))
         (journal (expand-file-name "sub/journal.org" dir))
         (text "* TODO Removed task\n:PROPERTIES:\n:ID:       6X1\n:END:\n#+begin_src sh\necho hi\n#+end_src\nLine.\n"))
    (unwind-protect
        (progn
          (org-sync-safety-journal-record journal :title "Deleted in Todoist" :text text
                                          :kind "deleted-subtree" :file "Home.org"
                                          :outline '("Garden" "Removed task")
                                          :id "6X1" :reason "Deleted\nremotely" :cycle "abc")
          (org-sync-safety-journal-record journal :title "Second" :text "Body")
          (with-temp-buffer
            (insert-file-contents journal)
            (org-mode)
            (goto-char (point-min))
            (re-search-forward "^\\* .*Deleted in Todoist$")
            (let ((entry (org-sync-safety-journal-entry)))
              (should (equal (plist-get entry :text) text))
              (should (equal (plist-get entry :kind) "deleted-subtree"))
              (should (equal (plist-get entry :file) "Home.org"))
              (should (equal (plist-get entry :outline) "Garden / Removed task"))
              (should (equal (plist-get entry :id) "6X1"))
              (should (equal (plist-get entry :reason) "Deleted remotely")))
            ;; The removed heading did not become a heading of the journal.
            (goto-char (point-min))
            (should (= 2 (cl-loop while (re-search-forward "^\\* " nil t) count t)))))
      (delete-directory dir t))))

(ert-deftest org-sync-safety/trash-moves-instead-of-deleting ()
  "Trashed files keep their relative path under a dated directory; names never clash."
  (let* ((root (make-temp-file "org-sync-safety-trash-" t))
         (trash (expand-file-name "trash" root)))
    (unwind-protect
        (let ((file (expand-file-name "data/ab/doc.pdf" root)))
          (make-directory (file-name-directory file) t)
          (write-region "one" nil file nil 'silent)
          (let ((first (org-sync-safety-trash-file file trash root)))
            (should-not (file-exists-p file))
            (should (string-match-p "/trash/[0-9-]+/data/ab/doc\\.pdf\\'" first))
            (write-region "two" nil file nil 'silent)
            (let ((second (org-sync-safety-trash-file file trash root)))
              (should (string-suffix-p ".2" second))
              (should (file-exists-p first))
              (should (file-exists-p second)))))
      (delete-directory root t))))

(ert-deftest org-sync-safety/region-guard ()
  "Changes inside the region pass; one outside signals after the body ran."
  (with-temp-buffer
    (insert "* One\nbody one\n* Two\nbody two\n")
    (let ((hooks-before (copy-sequence after-change-functions))
          (beg (point-min))
          (end (progn (goto-char (point-min)) (re-search-forward "^\\* Two") (match-beginning 0))))
      ;; Inside, including an insertion at the end boundary.
      (org-sync-safety-with-region-guard beg end "Inside"
        (goto-char (point-min))
        (re-search-forward "body one")
        (insert " and more")
        (goto-char (point-min))
        (re-search-forward "^\\* Two")
        (goto-char (match-beginning 0))
        (insert "\n"))
      ;; Outside: the change happens, then the guard signals.
      (let* ((end (save-excursion (goto-char (point-min)) (re-search-forward "^\\* Two")
                                  (match-beginning 0)))
             (err (should-error
                   (org-sync-safety-with-region-guard (point-min) end "Smear"
                     (goto-char (point-max))
                     (insert "smeared\n"))
                   :type 'org-sync-safety-violation)))
        (should (string-match-p "Smear changed text outside" (cadr err))))
      ;; The recorder is gone, also after a violation.
      (should (equal after-change-functions hooks-before)))))

(defvar org-sync-safety-test--hook nil "A hook for the unguarded-hooks test.")

(ert-deftest org-sync-safety/user-hooks-are-exempt ()
  "Changes a user hook makes elsewhere pass; the guarded code's own do not."
  (with-temp-buffer
    (insert "* One\nbody\n* Two\nbody\n")
    (let ((end (save-excursion (goto-char (point-min)) (re-search-forward "^\\* Two")
                               (match-beginning 0)))
          (org-sync-safety-test--hook
           (list (lambda () (save-excursion (goto-char (point-max)) (insert "triggered\n"))))))
      (org-sync-safety-with-region-guard (point-min) end "Hooked"
        (org-sync-safety-with-unguarded-hooks '(org-sync-safety-test--hook)
          (goto-char (point-min))
          (insert "x")
          (run-hooks 'org-sync-safety-test--hook)))
      (should (string-match-p "triggered" (buffer-string)))
      (should-error
       (org-sync-safety-with-region-guard (point-min) end "Unhooked"
         (org-sync-safety-with-unguarded-hooks '(org-sync-safety-test--hook)
           (goto-char (point-max))
           (insert "own change\n")))
       :type 'org-sync-safety-violation))))

(ert-deftest org-sync-safety/compare-ids ()
  "Only allowed IDs may vanish, and no ID may be held twice."
  (let ((before (make-hash-table :test 'equal))
        (after (make-hash-table :test 'equal))
        (allowed (make-hash-table :test 'equal)))
    (puthash "A" '("X.org") before)
    (puthash "B" '("X.org") before)
    (puthash "C" '("X.org") before)
    (puthash "M" '("X.org") before)
    (puthash "A" '("X.org") after)
    (puthash "M" '("Y.org") after)        ; moved across files: fine
    (puthash "D" '("X.org" "Y.org") after) ; duplicated
    (puthash "B" t allowed)
    (should (equal (org-sync-safety-compare-ids before after allowed)
                   '("C vanished from X.org"
                     "D is held 2 times (X.org, Y.org)")))))

(ert-deftest org-sync-safety/shrunk-files ()
  "A large file losing most of its size is suspicious unless its removal was intended."
  (let ((before (make-hash-table :test 'equal))
        (after (make-hash-table :test 'equal)))
    (puthash "Big.org" 10000 before)
    (puthash "Big.org" 3000 after)
    (puthash "Gone.org" 10000 before)
    (puthash "Small.org" 100 before)
    (puthash "Small.org" 1 after)
    (puthash "Fine.org" 10000 before)
    (puthash "Fine.org" 9000 after)
    (should (equal (org-sync-safety-shrunk-files before after '("Gone.org"))
                   '("Big.org shrank from 10000 to 3000 bytes")))))

(provide 'test-safety)
;;; test-safety.el ends here
