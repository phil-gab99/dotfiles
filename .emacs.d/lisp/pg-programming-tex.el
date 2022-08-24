(require 'pg-startup)

(use-package tex
  :straight auctex
  :config
  (require 'tex-site)
  (add-to-list 'auto-mode-alist '("\\.tex$" . LaTeX-mode))
  (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)
  (add-hook 'TeX-mode-hook (lambda () (run-hooks 'prog-mode-hook)))
  (put 'TeX-mode 'derived-mode-parent 'prog-mode)
  :custom
  (latex-run-command "pdflatex")
  (TeX-view-program-selection '((output-pdf "PDF Tools")))
  (TeX-source-correlate-start-server t))

(use-package company-auctex
  :straight t
  :after (auctex company)
  :config
  (add-to-list 'company-backends '(company-auctex :with company-yasnippet)))

(provide 'pg-programming-tex)
