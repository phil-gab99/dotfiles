(use-package lsp-css
  :straight nil
  :after lsp-mode
  :hook
  ((css-mode
    less-css-mode
    scss-mode) . lsp-deferred))

(provide 'pg-programming-css)
