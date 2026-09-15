;;; test-confirm.el --- Regression tests for the write-back review adapter -*- lexical-binding: t; -*-

;; Usage: emacs --batch -L . -l ert -l test-confirm.el -f ert-run-tests-batch-and-exit
;;
;; Covers orgist-confirm.el: building the org-sync-confirm tree from
;; diff results (labels, kinds, fields, payloads, live remote values
;; and the "changed in Todoist" warning), and executing a partial
;; selection (commands regenerated for the ticked changes, stamps
;; dropped so the rest stays pending).

(add-to-list 'load-path (expand-file-name "~/.emacs.d/elpaca/builds/request"))
;; org-sync-confirm is required by orgist-confirm; prefer the elpaca build,
;; fall back to the source checkout on a fresh clone.
(dolist (dir '("~/.emacs.d/elpaca/builds/org-sync-confirm"
               "~/.emacs.d/elpaca/sources/org-sync-confirm"))
  (when (file-directory-p (expand-file-name dir))
    (add-to-list 'load-path (expand-file-name dir))))
(add-to-list 'load-path default-directory)
(require 'ert)
(require 'cl-lib)
(require 'orgist)

(defconst orgist-test-confirm--project
  ":PROPERTIES:
:ID:       PROJ1
:END:
#+TITLE: Family

* TODO Renew passport
:PROPERTIES:
:ID:       T1
:END:
Expires in June.

Documents:
- form
- photo

* TODO Book photographer
:PROPERTIES:
:ID:       T2
:END:
Call the studio.
")

(defmacro orgist-test-confirm--with-project (&rest body)
  "Run BODY with a temporary Family.org project visited and snapshots set."
  (declare (indent 0))
  `(let* ((dir (make-temp-file "orgist-confirm-" t))
          (file (expand-file-name "Family.org" dir))
          (orgist-base-dir dir)
          (orgist-snapshots (make-hash-table :test 'equal))
          (orgist-sync-attachments nil)
          (orgist-sync-comments nil)
          (orgist-confirm-remote-fetch-limit 0)
          (buf nil))
     (unwind-protect
         (progn
           (with-temp-file file (insert orgist-test-confirm--project))
           (setq buf (find-file-noselect file))
           (with-current-buffer buf (org-mode))
           (puthash "T1" (list :content "Renew passport"
                               :description "Expires in June.\n\nDocuments:\n- form"
                               :priority 1 :parent-id "PROJ1")
                    orgist-snapshots)
           (puthash "T3" (list :content "Old task" :description "Gone soon"
                               :parent-id "PROJ1")
                    orgist-snapshots)
           ,@body)
       (when (buffer-live-p buf)
         (set-buffer-modified-p nil)
         (kill-buffer buf))
       (delete-directory dir t))))

(defconst orgist-test-confirm--changes
  '(("T1" . ((:description . ("Expires in June.\n\nDocuments:\n- form"
                              . "Expires in June.\n\nDocuments:\n- form\n- photo"))
             (:priority . (1 . 3))))
    ("T2" . new)
    ("T3" . deleted)))

(defconst orgist-test-confirm--commands
  '(((type . "item_update") (uuid . "u1") (args . ((id . "T1") (description . "md") (priority . 3))))
    ((type . "item_add") (uuid . "u2") (temp_id . "T2") (args . ((content . "Book photographer"))))
    ((type . "item_delete") (uuid . "u3") (args . ((id . "T3"))))
    ((type . "label_add") (uuid . "u4") (temp_id . "L1") (args . ((name . "urgent"))))))

(defun orgist-test-confirm--find (items label)
  "Return the item with LABEL anywhere in ITEMS."
  (let ((found nil))
    (org-sync-confirm--walk items
                            (lambda (i) (when (equal (plist-get i :label) label)
                                          (setq found i))))
    found))

(ert-deftest orgist-confirm/tree-from-changes ()
  "Changes become project containers with labelled, typed items."
  (orgist-test-confirm--with-project
    (let* ((items (orgist-confirm--items orgist-test-confirm--changes
                                         orgist-test-confirm--commands))
           (family (car items))
           (modified (orgist-test-confirm--find items "Renew passport"))
           (new (orgist-test-confirm--find items "Book photographer"))
           (deleted (orgist-test-confirm--find items "Old task"))
           (batch (orgist-test-confirm--find items "Batch")))
      (should (equal '("Batch" "Family") (sort (mapcar (lambda (i) (plist-get i :label)) items)
                                               #'string<)))
      (should (equal "Family" (plist-get family :label)))
      (should (= 3 (length (plist-get family :children))))
      (should (eq 'modified (plist-get modified :kind)))
      (should (equal '(("description" "Expires in June.\n\nDocuments:\n- form"
                        "Expires in June.\n\nDocuments:\n- form\n- photo")
                       ("priority" "p1" "p3"))
                     (mapcar (lambda (f) (list (plist-get f :name) (plist-get f :old)
                                               (plist-get f :new)))
                             (plist-get modified :fields))))
      (should (null (plist-get modified :warning)))
      (should (string-match-p "item_update" (plist-get modified :payload)))
      (should (equal (assoc "T1" orgist-test-confirm--changes) (plist-get modified :data)))
      (should (eq 'new (plist-get new :kind)))
      (should (equal '(("title" . "Book photographer") ("description" . "Call the studio."))
                     (mapcar (lambda (f) (cons (plist-get f :name) (plist-get f :new)))
                             (plist-get new :fields))))
      (should (string-match-p "item_add" (plist-get new :payload)))
      (should (eq 'deleted (plist-get deleted :kind)))
      (should (equal "permanent" (plist-get deleted :warning)))
      (should (equal '((:name "description" :old "Gone soon")) (plist-get deleted :fields)))
      (should (string-match-p "item_delete" (plist-get deleted :payload)))
      (let ((label-item (car (plist-get batch :children))))
        (should (equal "label_add urgent" (plist-get label-item :label)))
        (should (plist-get label-item :fixed))
        (should (null (plist-get label-item :data)))))))

(ert-deftest orgist-confirm/remote-value-replaces-old-and-warns ()
  "The live Todoist value is the old side; a drift from the snapshot warns."
  (orgist-test-confirm--with-project
    (let ((orgist-confirm-remote-fetch-limit 5)
          (fetched nil))
      (cl-letf (((symbol-function 'orgist-confirm--fetch-remote)
                 (lambda (id _section-p)
                   (push id fetched)
                   '((content . "Renew passport")
                     (description . "Expires in June.\n\nDocuments:\n- form\n- stamp")
                     (priority . 2)
                     (labels . ["Admin"])
                     (checked . :json-false))))
                ((symbol-function 'orgist-convert-description) (lambda (d _level) d)))
        (let* ((items (orgist-confirm--items orgist-test-confirm--changes
                                             orgist-test-confirm--commands))
               (modified (orgist-test-confirm--find items "Renew passport"))
               (fields (plist-get modified :fields)))
          (should (equal '("T1") fetched))
          (should (equal "Expires in June.\n\nDocuments:\n- form\n- stamp"
                         (plist-get (car fields) :old)))
          (should (equal "p2" (plist-get (cadr fields) :old)))
          (should (equal "changed in Todoist since last sync: description, priority"
                         (plist-get modified :warning))))))))

(ert-deftest orgist-confirm/remote-fetch-respects-limit ()
  "Above the limit nothing is fetched and snapshots are used."
  (orgist-test-confirm--with-project
    (let ((orgist-confirm-remote-fetch-limit 0)
          (fetched nil))
      (cl-letf (((symbol-function 'orgist-confirm--fetch-remote)
                 (lambda (id _) (push id fetched) nil)))
        (orgist-confirm--items orgist-test-confirm--changes orgist-test-confirm--commands)
        (should (null fetched))))))

(ert-deftest orgist-confirm/remote-state-shape ()
  "A REST task alist converts into the snapshot plist shape."
  (cl-letf (((symbol-function 'orgist-convert-description) (lambda (d _level) (upcase d)))
            ((symbol-function 'orgist--label-to-tag) (lambda (l) (concat "tag-" l))))
    (let ((state (orgist-confirm--remote-state
                  '((content . "Title") (description . "body") (priority . 4)
                    (labels . ["a" "b"]) (checked . t)
                    (due . ((date . "2026-10-01") (string . "Oct 1")))
                    (deadline . ((date . "2026-10-05"))))
                  nil 1)))
      (should (equal "Title" (plist-get state :content)))
      (should (equal "BODY" (plist-get state :description)))
      (should (= 4 (plist-get state :priority)))
      (should (equal '("tag-a" "tag-b") (plist-get state :labels)))
      (should (eq t (plist-get state :checked)))
      (should (string-match-p "2026-10-01" (plist-get state :due)))
      (should (string-match-p "2026-10-05" (plist-get state :deadline))))))

(ert-deftest orgist-confirm/command-element-ids ()
  "Commands map back to the element they act on."
  (should (equal "T1" (orgist-confirm--command-element-id
                       '((type . "item_update") (args . ((id . "T1")))))))
  (should (equal "T2" (orgist-confirm--command-element-id
                       '((type . "item_add") (temp_id . "T2") (args . ((content . "x")))))))
  (should (equal "T1" (orgist-confirm--command-element-id
                       '((type . "note_add") (temp_id . "N1") (args . ((item_id . "T1")))))))
  (should (equal "T4" (orgist-confirm--command-element-id
                       '((type . "item_reorder") (args . ((items . [((id . "T4") (child_order . 1))])))))))
  (should (null (orgist-confirm--command-element-id
                 '((type . "label_add") (temp_id . "L1") (args . ((name . "x"))))))))

(ert-deftest orgist-confirm/field-values ()
  "Absent values are nil except for the checkbox; present ones are formatted."
  (should (null (orgist-confirm--field-value :description nil)))
  (should (null (orgist-confirm--field-value :description "  \n")))
  (should (null (orgist-confirm--field-value :due nil)))
  (should (equal "☐" (orgist-confirm--field-value :checked nil)))
  (should (equal "☑" (orgist-confirm--field-value :checked t)))
  (should (equal "{a, b}" (orgist-confirm--field-value :labels '("a" "b"))))
  (should (equal "p3" (orgist-confirm--field-value :priority 3)))
  (should (equal "(none)" (orgist-confirm--format-value :attachment-files nil))))

(ert-deftest orgist-confirm/full-selection-sends-original-batch ()
  "All items ticked: the previewed commands are sent and stamps are kept."
  (let ((sent nil)
        (orgist--pending-stamps '(("f.org" . "hash"))))
    (cl-letf (((symbol-function 'orgist-execute-write-back) (lambda (cmds) (setq sent cmds)))
              ((symbol-function 'orgist-changes-to-commands)
               (lambda (_) (error "Must not regenerate"))))
      (orgist-confirm--execute (mapcar (lambda (c) (list :data c)) orgist-test-confirm--changes)
                               3 orgist-test-confirm--commands))
    (should (eq sent orgist-test-confirm--commands))
    (should (equal '(("f.org" . "hash")) orgist--pending-stamps))))

(ert-deftest orgist-confirm/partial-selection-regenerates-and-keeps-files-due ()
  "A subset regenerates commands for the ticked changes and drops the stamps."
  (let ((sent nil)
        (regenerated-for nil)
        (orgist--pending-stamps '(("f.org" . "hash"))))
    (cl-letf (((symbol-function 'orgist-execute-write-back) (lambda (cmds) (setq sent cmds)))
              ((symbol-function 'orgist-changes-to-commands)
               (lambda (changes) (setq regenerated-for changes) '(regenerated)))
              ((symbol-function 'orgist-log) #'ignore))
      (orgist-confirm--execute (list (list :data (assoc "T2" orgist-test-confirm--changes))
                                     (list :label "label_add" :fixed t))
                               3 orgist-test-confirm--commands))
    (should (equal '(regenerated) sent))
    (should (equal (list (assoc "T2" orgist-test-confirm--changes)) regenerated-for))
    (should (null orgist--pending-stamps))))

(ert-deftest orgist-confirm/cancel-clears-mutex ()
  "Cancelling releases the sync mutex so the next sync can run."
  (let ((orgist-sync-mutex (current-time)))
    (cl-letf (((symbol-function 'orgist-log) #'ignore))
      (orgist-confirm--cancel))
    (should (null orgist-sync-mutex))))

(provide 'test-confirm)

;;; test-confirm.el ends here
