;;; test-isolation.el --- Keep test runs away from the user's files -*- lexical-binding: t; -*-

;; Every test file requires this before Org and orgist load.
;;
;; `emacs --batch' skips the init file but keeps the real
;; `user-emacs-directory', and orgist derives its defaults from it: a
;; suite that did not bind `orgist-base-dir' wrote its log, journal and
;; history into the user's mirror directory (seen 2026-10-03), and
;; org-id and org-persist save into ~/.emacs.d on exit.  Here
;; `user-emacs-directory' is a sandbox, deleted when Emacs exits, so
;; every such default lands in it.  Suites that need a base directory
;; of their own still bind one.

(when (featurep 'orgist)
  (error "test-isolation.el must load before orgist"))

(defvar orgist-test-sandbox
  (file-name-as-directory (make-temp-file "orgist-test-" t))
  "Throwaway directory standing in for `user-emacs-directory'.")

(setq user-emacs-directory orgist-test-sandbox
      org-id-locations-file (expand-file-name ".org-id-locations" orgist-test-sandbox)
      org-persist-directory (expand-file-name "org-persist/" orgist-test-sandbox))

(with-eval-after-load 'orgist
  (unless (string-prefix-p (expand-file-name orgist-test-sandbox)
                           (expand-file-name orgist-base-dir))
    (error "orgist-base-dir %s is outside the test sandbox" orgist-base-dir)))

;; Last on the hook: org-id and org-persist save into the sandbox first.
(add-hook 'kill-emacs-hook
          (lambda () (ignore-errors (delete-directory orgist-test-sandbox t)))
          100)

(provide 'test-isolation)
;;; test-isolation.el ends here
