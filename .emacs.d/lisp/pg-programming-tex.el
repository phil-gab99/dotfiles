;;; pg-programming-tex.el -*- lexical-binding: t; -*-
;; Author: Philippe Gabriel

(straight-use-package 'auctex)
(require 'tex)
(with-eval-after-load 'tex
  (pg/customize-set-variables
   '((latex-run-command . "pdflatex")
     (TeX-view-program-selection . ((output-pdf "PDF Tools")))
     (TeX-source-correlate-start-server . t)))
  (put 'tex-mode 'derived-mode-parent 'prog-mode)
  (unless (fboundp 'latex-mode)
    (autoload #'latex-mode "tex-mode" nil t))
  (add-to-list 'auto-mode-alist '("\\.tex$" . latex-mode))
  (add-hook 'TeX-mode-hook #'(lambda nil (run-hooks 'prog-mode-hook)))
  (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer))

;; (straight-use-package 'company-auctex)
;; (with-eval-after-load 'company
;;   (with-eval-after-load 'tex
;;     (require 'company-auctex)))
;; (with-eval-after-load 'company-auctex
;;   (add-to-list 'company-backends '(company-auctex :with company-yasnippet)))

(provide 'pg-programming-tex)