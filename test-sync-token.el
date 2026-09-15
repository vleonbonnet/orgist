;;; test-sync-token.el --- Regression tests for sync cursor safety -*- lexical-binding: t; -*-

;; Usage: emacs --batch -L . -l ert -l test-sync-token.el -f ert-run-tests-batch-and-exit
;;
;; A commands-only Sync API response contains a fresh sync token but no
;; resource changes.  Saving that token loses any remote changes between the
;; previous read cursor and the command response.  Only a pull that has
;; successfully applied its returned resources may advance the read cursor.

(add-to-list 'load-path
             (expand-file-name "~/.emacs.d/elpaca/builds/request"))
;; org-sync-confirm is required by orgist-confirm; prefer the elpaca build,
;; fall back to the source checkout on a fresh clone.
(dolist (dir '("~/.emacs.d/elpaca/builds/org-sync-confirm"
               "~/.emacs.d/elpaca/sources/org-sync-confirm"))
  (when (file-directory-p (expand-file-name dir))
    (add-to-list 'load-path (expand-file-name dir))))
(add-to-list 'load-path default-directory)
(require 'ert)
(require 'orgist)

(ert-deftest orgist-sync-token/write-response-does-not-advance-read-cursor ()
  "A commands-only response must not persist its sync token."
  (let ((saved-tokens nil)
        (orgist-write-back-batch-size 100))
    (cl-letf (((symbol-function 'orgist--send-command-chunk)
               (lambda (_commands)
                 '(:sync-status ((command-uuid . "ok"))
                   :temp-id-mapping nil
                   :sync-token "unapplied-command-response-token")))
              ((symbol-function 'orgist-save-sync-token)
               (lambda (token)
                 (push token saved-tokens))))
      (let ((result
             (orgist-send-commands
              '(((type . "item_update")
                 (uuid . "command-uuid")
                 (args . ((id . "task-id") (content . "Updated"))))))))
        (should (equal '((command-uuid . "ok"))
                       (plist-get result :sync-status)))
        (should-not saved-tokens)))))

(provide 'test-sync-token)
;;; test-sync-token.el ends here
