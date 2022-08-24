(require 'pg-startup)

(use-package haskell-mode
  :straight t
  :hook ((haskell-mode haskell-literate-mode) . lsp-deferred))

(use-package lsp-haskell
  :disabled ;; Not working on Haskell recently
  :custom
  (lsp-haskell-server-path "~/.ghcup/bin/haskell-language-server-8.10.6"))

(provide 'pg-programming-haskell)
