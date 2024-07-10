;;; pg-programming-tex.el -*- lexical-binding: t; -*-
;; Author: Philippe Gabriel

(straight-use-package 'auctex)
(add-hook 'TeX-mode-hook #'(lambda nil (run-hooks 'prog-mode-hook)))
(unless (fboundp 'TeX-revert-document-buffer)
  (autoload #'TeX-revert-document-buffer "tex-mode" nil t))
(add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)
(unless (fboundp 'latex-mode)
  (autoload #'latex-mode "tex-mode" nil t))
(add-to-list 'auto-mode-alist '("\\.tex\\'" . latex-mode))
(with-eval-after-load 'tex
  (pg/customize-set-variables
   '((latex-run-command . "pdflatex")
     (TeX-view-program-selection . ((output-pdf "PDF Tools")))
     (TeX-source-correlate-start-server . t)))
  (put 'tex-mode 'derived-mode-parent 'prog-mode))

(straight-use-package 'company-auctex)

(provide 'pg-programming-tex)
