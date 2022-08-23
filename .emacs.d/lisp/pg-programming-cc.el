(use-package cc-mode
  :straight nil
  :config
  (setq c-basic-offset 4)
  :custom
  (company-clang-executable (expand-file-name "~/.guix-extra-profiles/cc/cc/bin/clang"))
  :hook ((c-mode c++-mode objc-mode) . lsp-deferred))

(use-package company-c-headers
  :straight t
  :after (cc-mode company)
  :config
  (add-to-list 'company-backends '(company-c-headers :with company-yasnippet)))

(use-package ccls
  :straight t)

(provide 'pg-programming-cc)
