;;; test-local-links.el --- Local link conversion regressions -*- lexical-binding: t; -*-
(add-to-list 'load-path (expand-file-name "~/.emacs.d/elpaca/builds/request"))
(add-to-list 'load-path (file-name-directory (or load-file-name buffer-file-name)))
(require 'ert)
(require 'orgist)
(setq orgist-log-file nil)

(defun orgist-test--first-link (text)
  (with-temp-buffer
    (insert text)
    (org-mode)
    (org-element-map (org-element-parse-buffer) 'link #'identity nil t)))

(ert-deftest orgist-local-links/import-decodes-local-paths ()
  (skip-unless (executable-find "pandoc"))
  (dolist (spec '(("attachment:Timbre%20fiscal%202026.pdf" "attachment" "Timbre fiscal 2026.pdf")
                  ("attachment:%C3%89tiquette%20FedEx.pdf" "attachment" "Étiquette FedEx.pdf")
                  ("../Gaia/Acte%20de%20naissance%20français%202024.jpg" "file" "../Gaia/Acte de naissance français 2024.jpg")
                  ("file:../Documents/Photo%202026.jpg" "file" "../Documents/Photo 2026.jpg")
                  ("../../Downloads/Passeports%202026%20Temporaire/" "file" "../../Downloads/Passeports 2026 Temporaire/")
                  ("attachment:Literal%2520name.pdf" "attachment" "Literal%20name.pdf")))
    (let* ((result (orgist-convert-description (format "[Document](%s)" (car spec)) 2))
           (link (orgist-test--first-link result)))
      (should (equal (org-element-property :type link) (nth 1 spec)))
      (should (equal (org-element-property :path link) (nth 2 spec))))))

(ert-deftest orgist-local-links/preserves-web-urls-and-other-protocols ()
  (skip-unless (executable-find "pandoc"))
  (dolist (target '("https://example.com/Photo%20name?a=x%26y"
                    "id:some%20identifier"
                    "//example.com/Photo%20name"))
    (let* ((result (orgist-convert-description (format "[Document](%s)" target) 2))
           (link (orgist-test--first-link result)))
      (should (equal (org-element-property :raw-link link) target)))))

(ert-deftest orgist-local-links/passport-paths-survive-repeated-roundtrips ()
  (skip-unless (executable-find "pandoc"))
  (dolist (target '("attachment:Récapitulatif de pré demande 2026.pdf"
                    "attachment:Photo 35x45 mm 2026.jpg"
                    "file:../../Interceptor/Pictures/good_portraits/Passport application 2026-09-12.jpg"
                    "file:data/6X/task/Confirmation de rendez vous 2026-10-01.jpg"))
    (let* ((text (format "[[%s][Document]]" target))
           (initial (orgist-test--first-link text))
           (type (org-element-property :type initial))
           (path (org-element-property :path initial)))
      (dotimes (_ 3)
        (setq text (orgist-convert-description
                    (orgist-convert-description-to-markdown text) 2))
        (let ((link (orgist-test--first-link text)))
          (should (equal (org-element-property :type link) type))
          (should (equal (org-element-property :path link) path)))))))

(ert-deftest orgist-local-links/follow-attachment-resolves-existing-file ()
  (skip-unless (executable-find "pandoc"))
  (let* ((dir (make-temp-file "orgist-link-test-" t))
         (filename "Photo 35x45 mm 2026.jpg")
         (expected (expand-file-name filename dir)))
    (unwind-protect
        (progn
          (with-temp-file expected (insert "fixture"))
          (with-temp-buffer
            (org-mode)
            (insert "* TODO Passport\n:PROPERTIES:\n:DIR: " dir "\n:END:\n\n"
                    (orgist-convert-description "[Photo](attachment:Photo%2035x45%20mm%202026.jpg)" 1))
            (goto-char (point-min))
            (search-forward "[[attachment:")
            (let (opened)
              (cl-letf (((symbol-function 'org-open-file)
                         (lambda (path &rest _) (setq opened path))))
                (org-open-at-point))
              (should (equal opened expected))
              (should (file-exists-p opened)))))
      (delete-directory dir t))))
