(use-package typescript-mode
  :straight t
  :mode "\\.ts\\'"
  :hook (typescript-mode . lsp-deferred)
  :config
  (require 'dap-node)
  (dap-node-setup))

(provide 'pg-programming-typescript)
