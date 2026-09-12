;;; test-id-cache.el --- Regression tests for the element ID cache -*- lexical-binding: t; -*-

;; Usage: emacs --batch -L . -l ert -l test-id-cache.el -f ert-run-tests-batch-and-exit
;;
;; Locks in the repair path of `orgist-find-element-by-id' added after
;; the 2026-09-12 duplicate-heading incident: cache markers use
;; insertion-type t, so cutting a subtree and re-inserting it elsewhere
;; collapses its marker onto the following heading.  The lookup used to
;; report the element as absent, and the pull path then created a
;; second heading with the same :ID:.  A stale marker must now be
;; repaired by rescanning; an ID that was never cached must still be
;; answered without a scan (the presence check asks about every
;; snapshot ID in every file).

(add-to-list 'load-path
             (expand-file-name "~/.emacs.d/elpaca/builds/request"))
(add-to-list 'load-path default-directory)
(require 'ert)
(require 'cl-lib)
(require 'orgist)

(defconst orgist-test--three-tasks
  "* TODO A
:PROPERTIES:
:ID:       IDA
:END:

* TODO B
:PROPERTIES:
:ID:       IDB
:END:

* TODO C
:PROPERTIES:
:ID:       IDC
:END:

")

(defmacro orgist-test--with-cached-buffer (&rest body)
  "Run BODY in a temp org buffer holding three tasks with a built ID cache."
  (declare (indent 0))
  `(with-temp-buffer
     (insert orgist-test--three-tasks)
     (org-mode)
     (orgist-build-id-cache)
     (unwind-protect
         (progn ,@body)
       (set-buffer-modified-p nil))))

(defun orgist-test--move-b-after-c ()
  "Cut task B out and re-insert it after C with raw buffer surgery.
The deletion collapses B's marker onto C's heading; the insertion
happens elsewhere, so the marker stays on C — stale."
  (let* ((b-beg (progn (goto-char (point-min))
                       (re-search-forward "^\\* TODO B$")
                       (line-beginning-position)))
         (b-end (progn (re-search-forward "^\\* TODO C$")
                       (line-beginning-position)))
         (txt (buffer-substring b-beg b-end)))
    (delete-region b-beg b-end)
    (goto-char (point-max))
    (insert txt)))

(ert-deftest orgist-id-cache/stale-marker-repaired-by-rescan ()
  "A marker left on the wrong heading is repaired, not reported as absent."
  (orgist-test--with-cached-buffer
    (orgist-test--move-b-after-c)
    ;; Precondition: the cached marker really is stale.
    (should (equal (save-excursion
                     (goto-char (gethash "IDB" orgist-id-cache))
                     (org-entry-get (point) "ID"))
                   "IDC"))
    (let ((pos (orgist-find-element-by-id "IDB")))
      (should pos)
      (goto-char pos)
      (should (org-at-heading-p))
      (should (equal (org-entry-get (point) "ID") "IDB"))
      ;; The repaired marker serves the next lookup directly.
      (should (= (marker-position (gethash "IDB" orgist-id-cache)) pos))
      (should (= (orgist-find-element-by-id "IDB") pos)))
    ;; The neighbours were never disturbed.
    (should (orgist-find-element-by-id "IDA"))
    (should (orgist-find-element-by-id "IDC"))))

(ert-deftest orgist-id-cache/removed-heading-is-absent ()
  "A cached ID whose heading was deleted is reported absent and dropped."
  (orgist-test--with-cached-buffer
    (let* ((b-beg (progn (goto-char (point-min))
                         (re-search-forward "^\\* TODO B$")
                         (line-beginning-position)))
           (b-end (progn (re-search-forward "^\\* TODO C$")
                         (line-beginning-position))))
      (delete-region b-beg b-end))
    (should-not (orgist-find-element-by-id "IDB"))
    (should-not (gethash "IDB" orgist-id-cache))
    (should (orgist-find-element-by-id "IDC"))))

(ert-deftest orgist-id-cache/unknown-id-does-not-scan ()
  "An ID never cached for this buffer is answered without a buffer scan."
  (orgist-test--with-cached-buffer
    (let ((scans 0))
      (cl-letf (((symbol-function 'orgist--scan-for-element-id)
                 (lambda (_id) (cl-incf scans) nil)))
        (should-not (orgist-find-element-by-id "IDZ"))
        (should (= scans 0))))))

(provide 'test-id-cache)
;;; test-id-cache.el ends here
