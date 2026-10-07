;;; test-capture.el --- tee `message' output to a file for batch runs -*- lexical-binding: t; -*-
;; Preload before test-harness.el so harness `message' output is captured
;; even when native Windows Emacs won't write to redirected stderr.
;; Target file is passed via the ORGIST_TEST_CAPTURE env var.
(defvar orgist-test-capture--in-advice nil)
(let ((capture-file (getenv "ORGIST_TEST_CAPTURE")))
  (when capture-file
    (when (file-exists-p capture-file) (delete-file capture-file))
    (advice-add
     'message :before
     (lambda (fmt &rest args)
       (when (and fmt (not orgist-test-capture--in-advice))
         (let ((orgist-test-capture--in-advice t)
               (line (concat (apply #'format fmt args) "\n"))
               (inhibit-message t))
           (write-region line nil capture-file t 'silent)))))))
;;; test-capture.el ends here
