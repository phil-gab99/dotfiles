(require 'auctex)
(with-eval-after-load 'auctex
  (require 'tex-site)
  (add-to-list 'auto-mode-alist '("\\.tex$" . LaTeX-mode))
  (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)
  (add-hook 'TeX-mode-hook #'(lambda () (run-hooks 'prog-mode-hook)))
  (put 'TeX-mode 'derived-mode-parent 'prog-mode)
  (customize-set-variable 'latex-run-command "pdflatex")
  (customize-set-variable 'TeX-view-program-selection '((output-pdf "PDF Tools")))
  (customize-set-variable 'TeX-source-correlate-start-server t))

(with-eval-after-load 'company
  (with-eval-after-load 'auctex
    (require 'company-auctex)
    (with-eval-after-load 'company-auctex
      (add-to-list 'company-backends '(company-auctex :with company-yasnippet)))))

(provide 'pg-programming-tex)
