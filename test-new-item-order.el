;;; test-new-item-order.el --- Regression tests for new-task ordering -*- lexical-binding: t; -*-

;; Usage: emacs --batch -L . -l ert -l test-new-item-order.el -f ert-run-tests-batch-and-exit
;;
;; A task written between two synced siblings used to be pushed with
;; an item_add carrying no child_order.  Todoist appended it after its
;; siblings, and the next pull moved the heading to the bottom of its
;; parent, undoing the user's placement (seen 2026-09-12).  The scan
;; now numbers the new heading and its siblings by buffer position:
;; the item_add carries the new task's child_order, and every sibling
;; whose order shifted is re-diffed so its item_reorder goes out in
;; the same batch.

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

(defconst orgist-test--project-file
  ":PROPERTIES:
:ID:       PROJ1
:TODOIST-PROJECT:
:END:
#+TITLE: Proj
* TODO First
:PROPERTIES:
:ID:       T1
:TODOIST-ORDER: 0
:END:

* TODO Second
:PROPERTIES:
:ID:       T2
:TODOIST-ORDER: 1
:END:

")

(defmacro orgist-test--with-project (&rest body)
  "Run BODY with a throwaway orgist project directory as `orgist-base-dir'.
The project file holds two synced tasks whose snapshots match their
local state, so only the edits made by BODY can diff.  BODY runs in
the buffer visiting the project file."
  (declare (indent 0))
  `(let* ((dir (make-temp-file "orgist-test-" t))
          (file (expand-file-name "Proj.org" dir))
          (orgist-base-dir dir)
          (orgist-snapshot-file (expand-file-name "snapshots.el" dir))
          (orgist--write-back-stamps nil)
          (orgist--write-back-stamps-path nil)
          (orgist--pending-stamps nil)
          (orgist-snapshots (make-hash-table :test 'equal))
          (orgist-sync-comments nil)
          (orgist-sync-attachments nil)
          (orgist-enable-write-back t)
          (orgist-project-buffer-cache nil))
     (with-temp-file file (insert orgist-test--project-file))
     (let ((buf (find-file-noselect file)))
       (unwind-protect
           (with-current-buffer buf
             (orgist-build-id-cache)
             (dolist (id '("T1" "T2"))
               (goto-char (orgist-find-element-by-id id))
               (puthash id (orgist-element-local-state) orgist-snapshots))
             ,@body)
         (with-current-buffer buf
           (set-buffer-modified-p nil))
         (kill-buffer buf)
         (delete-directory dir t)))))

(defun orgist-test--insert-between ()
  "Write a new task between First and Second and save the file."
  (goto-char (orgist-find-element-by-id "T2"))
  (insert "* TODO Inserted\n\n")
  (save-buffer))

(defun orgist-test--order-of (heading)
  "Return the TODOIST-ORDER property of HEADING as a number."
  (goto-char (point-min))
  (re-search-forward (format "^\\* TODO %s$" (regexp-quote heading)))
  (string-to-number (org-entry-get (point) "TODOIST-ORDER")))

(ert-deftest orgist-new-item-order/scan-numbers-new-heading-and-siblings ()
  "The scan gives the new heading its positional order and shifts the sibling after it."
  (orgist-test--with-project
    (orgist-test--insert-between)
    (let ((changes (orgist-diff-all-elements)))
      (should (= (orgist-test--order-of "First") 0))
      (should (= (orgist-test--order-of "Inserted") 1))
      (should (= (orgist-test--order-of "Second") 2))
      ;; New heading detected, shifted sibling re-diffed, untouched one silent.
      (should (seq-find (lambda (c) (eq (cdr c) 'new)) changes))
      (should (equal (assq :order (alist-get "T2" changes nil nil #'equal))
                     '(:order 1 . 2)))
      (should-not (alist-get "T1" changes nil nil #'equal)))))

(ert-deftest orgist-new-item-order/item-add-carries-child-order ()
  "item_add places the task at its heading's position; the shifted sibling is reordered."
  (orgist-test--with-project
    (orgist-test--insert-between)
    (let* ((commands (orgist-changes-to-commands (orgist-diff-all-elements)))
           (add (seq-find (lambda (c) (equal (alist-get 'type c) "item_add"))
                          commands))
           (reorder (seq-find (lambda (c) (equal (alist-get 'type c) "item_reorder"))
                              commands)))
      (should add)
      (should (equal (alist-get 'content (alist-get 'args add)) "Inserted"))
      (should (equal (alist-get 'child_order (alist-get 'args add)) 1))
      (should reorder)
      (let ((item (aref (alist-get 'items (alist-get 'args reorder)) 0)))
        (should (equal (alist-get 'id item) "T2"))
        (should (equal (alist-get 'child_order item) 2)))
      ;; First keeps order 0: no command for it.
      (should-not (seq-find (lambda (c)
                              (and (equal (alist-get 'type c) "item_reorder")
                                   (equal (alist-get 'id (aref (alist-get 'items (alist-get 'args c)) 0))
                                          "T1")))
                            commands)))))

(ert-deftest orgist-new-item-order/appended-heading-shifts-nothing ()
  "A task written after the last sibling takes the next order and touches no sibling."
  (orgist-test--with-project
    (goto-char (point-max))
    (insert "* TODO Appended\n\n")
    (save-buffer)
    (let ((changes (orgist-diff-all-elements)))
      (should (= (orgist-test--order-of "Appended") 2))
      (should (= (length changes) 1))
      (should (eq (cdr (car changes)) 'new)))))

(ert-deftest orgist-new-item-order/renumbering-resolves-tied-orders ()
  "Siblings sharing one stored order are renumbered 0..n by position,
every changed sibling but the one at point is reported, and writes
that widen a value do not disturb the siblings after it."
  (with-temp-buffer
    ;; A project file starts with its property drawer, so top-level
    ;; siblings are collected from before the first heading.
    (insert ":PROPERTIES:\n:ID:       PROJ1\n:TODOIST-PROJECT:\n:END:\n#+TITLE: Proj\n")
    ;; Thirteen siblings all stored as order 0 (Todoist ties): the
    ;; eleventh onwards get two-digit values.
    (dotimes (i 13)
      (insert (format "* TODO S%d\n:PROPERTIES:\n:ID:       S%d\n:TODOIST-ORDER: 0\n:END:\n\n"
                      i i)))
    (org-mode)
    (goto-char (point-min))
    (re-search-forward "^\\* TODO S0$")
    (org-back-to-heading t)
    (let ((changed (orgist--renumber-siblings-at-point)))
      (should (= (length changed) 12))
      (dotimes (i 13)
        (goto-char (point-min))
        (re-search-forward (format "^\\* TODO S%d$" i))
        (should (equal (org-entry-get (point) "TODOIST-ORDER")
                       (number-to-string i)))))
    (set-buffer-modified-p nil)))

(provide 'test-new-item-order)
;;; test-new-item-order.el ends here
