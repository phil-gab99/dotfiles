;;; pg-programming-typescript.el -*- lexical-binding: t; -*-
;; Author: Philippe Gabriel

(use-package typescript-mode
  :straight t
  :init
  (require 'typescript-mode)
  :mode "\\.ts$")

(use-package dap-node
  :straight nil
  :after (typescript-mode lsp-mode)
  :hook
  (typescript-mode . lsp-deferred)
  :config
  (dap-node-setup))

(provide 'pg-programming-typescript)
