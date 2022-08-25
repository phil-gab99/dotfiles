(with-eval-after-load 'lsp-mode
  (require 'haskell-mode)
  (with-eval-after-load 'haskell-mode
    (dolist (mode '(haskell-mode
                    haskell-literate-mode))
      (add-hook mode #'lsp-deferred))))

(straight-use-package 'lsp-haskell)
(with-eval-after-load 'lsp-mode
  (require 'lsp-haskell))

(provide 'pg-programming-haskell)
