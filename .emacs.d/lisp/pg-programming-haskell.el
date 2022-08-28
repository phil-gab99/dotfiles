;;; pg-programming-haskell.el -*- lexical-binding: t; -*-
;; Author: Philippe Gabriel

(use-package haskell-mode
  :straight t
  :init
  (require 'haskell-mode))

(use-package lsp-haskell
  :disabled
  :straight t
  :init
  (require 'lsp-haskell)
  :after lsp-mode
  :hook
  ((haskell-mode
    haskell-literate-mode) . lsp-deferred))

(provide 'pg-programming-haskell)
