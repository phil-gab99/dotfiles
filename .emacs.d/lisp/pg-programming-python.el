(use-package lsp-python-ms
  :straight t
  :after (python lsp-mode)
  :hook
  (python-mode . lsp-deferred)
  :custom
  (lsp-python-ms-auto-install-server t))

(use-package dap-python
  :straight nil
  :after (python lsp-mode)
  :custom
  (dap-python-debugger 'debugpy))

(use-package jupyter
  :disabled
  :straight t)

(provide 'pg-programming-python)
