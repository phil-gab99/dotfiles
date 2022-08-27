;;; pg-programming-python.el -*- lexical-binding: t; -*-
;; Author: Philippe Gabriel

(use-package lsp-python-ms
  :straight t
  :init
  (require 'lsp-python-ms)
  :after (python lsp-mode)
  :hook
  (python-mode . lsp-deferred)
  :custom
  (lsp-python-ms-auto-install-server t))

(use-package dap-python
  :straight nil
  :init
  (require 'dap-python)
  :after (python lsp-mode)
  :custom
  (dap-python-debugger 'debugpy))

(use-package jupyter
  :disabled
  :straight t
  :init
  (require 'jupyter))

(provide 'pg-programming-python)
