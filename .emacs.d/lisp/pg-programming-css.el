(require 'pg-startup)

(use-package lsp-css
  :straight nil
  :hook ((css-mode less-css-mode scss-mode) . lsp-deferred))

(provide 'pg-programming-css)
