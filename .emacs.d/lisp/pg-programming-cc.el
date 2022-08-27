;;; pg-programming-cc.el -*- lexical-binding: t; -*-
;; Author: Philippe Gabriel

(use-package cc-mode
  :straight nil
  :init
  (require 'cc-mode)
  :hook
  ((c-mode
    c++-mode
    objc-mode) . lsp-deferred)
  :custom
  (company-clang-executable (expand-file-name "~/.guix-extra-profiles/cc/cc/bin/clang")))

(use-package cc-vars
  :straight nil
  :init
  (require 'cc-vars)
  :after cc-mode
  :custom
  (c-basic-offset 4))

(use-package company-c-headers
  :straight t
  :init
  (require 'company-c-headers)
  :after (cc-mode company)
  :config
  (add-to-list 'company-backends '(company-c-headers :with company-yasnippet)))

(use-package ccls
  :straight t
  :init
  (require 'ccls)
  :after (cc-mode lsp-mode))

(provide 'pg-programming-cc)
