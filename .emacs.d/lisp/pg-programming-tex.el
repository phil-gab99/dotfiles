(use-package tex
  :straight auctex
  :mode ("\\.tex$" . latex-mode)
  :hook
  (TeX-mode-hook . (lambda ()
                     (run-hooks 'prog-mode-hook)))
  :custom
  (latex-run-command "pdflatex")
  (TeX-view-program-selection '((output-pdf "PDF Tools")))
  (TeX-source-correlate-start-server t)
  :config
  (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)
  (put 'tex-mode 'derived-mode-parent 'prog-mode))

(use-package company-auctex
  :straight t
  :after (company auctex)
  :config
  (add-to-list 'company-backends '(company-auctex :with company-yasnippet)))

(provide 'pg-programming-tex)
