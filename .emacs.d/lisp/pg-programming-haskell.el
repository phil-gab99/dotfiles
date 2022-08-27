(use-package haskell-mode
  :straight t)

(use-package lsp-haskell
  :straight t
  :after lsp-mode
  :hook
  ((haskell-mode
    haskell-literate-mode) . lsp-deferred))

(provide 'pg-programming-haskell)
