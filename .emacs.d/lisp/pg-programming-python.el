;;; pg-programming-python.el -*- lexical-binding: t; -*-
;; Author: Philippe Gabriel

(straight-use-package 'lsp-pyright)
(with-eval-after-load 'python
  (with-eval-after-load 'lsp-mode
    (require 'lsp-pyright)))
(with-eval-after-load 'lsp-pyright
  (add-hook 'python-mode #'lsp-deferred))

(with-eval-after-load 'python
  (with-eval-after-load 'lsp-mode
    (require 'dap-python)))
(with-eval-after-load 'dap-python
  (customize-set-variable 'dap-python-debugger 'debugpy))

;; (straight-use-package 'jupyter)

(provide 'pg-programming-python)
