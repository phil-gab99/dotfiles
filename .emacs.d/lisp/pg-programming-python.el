(require 'python)
(with-eval-after-load 'python
  (add-hook 'python-mode-hook #'lsp-deferred)
  (require 'dap-python)
  (customize-set-variable 'dap-python-debugger 'debugpy))

(straight-use-package 'lsp-python-ms)
(with-eval-after-load 'lsp-mode
  (require 'lsp-python-ms)
  (with-eval-after-load 'lsp-python-ms
    (add-hook 'python-mode-hook #'lsp-deferred)
    (customize-set-variable 'lsp-python-ms-auto-install-server t)))

;; (require 'jupyter)

(provide 'pg-programming-python)
