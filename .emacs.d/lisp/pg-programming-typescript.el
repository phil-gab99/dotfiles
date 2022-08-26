(straight-use-package 'typescript-mode)
(with-eval-after-load 'lsp
  (require 'typescript-mode)
  (with-eval-after-load 'typescript-mode
    (add-hook 'typescript-mode-hook #'lsp-deferred)
    (add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode))
    (require 'dap-node)
    (with-eval-after-load 'dap-node
      (dap-node-setup))))

(provide 'pg-programming-typescript)
