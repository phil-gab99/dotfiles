(use-package cc-mode
  :straight nil
  :hook
  ((c-mode
    c++-mode
    objc-mode) . lsp-deferred)
  :custom
  (company-clang-executable (expand-file-name "~/.guix-extra-profiles/cc/cc/bin/clang")))

(use-package cc-vars
  :straight nil
  :after cc-mode
  :custom
  (c-basic-offset 4))

(use-package company-c-headers
  :straight t
  :after (cc-mode company)
  :config
  (add-to-list 'company-backends '(company-c-headers :with company-yasnippet)))

(use-package ccls
  :straight t
  :after (cc-mode lsp-mode))

(provide 'pg-programming-cc)
