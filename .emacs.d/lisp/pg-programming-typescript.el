;;; pg-programming-typescript.el -*- lexical-binding: t; -*-
;; Author: Philippe Gabriel

(straight-use-package 'typescript-mode)
(unless (fboundp 'typescript-mode)
  (autoload #'typescript-mode "typescript-mode" nil t))
(add-to-list 'auto-mode-alist '("\\.ts$" . typescript-mode))

(with-eval-after-load 'typescript-mode
  (with-eval-after-load 'lsp-mode
    (require 'dap-node)))
(with-eval-after-load 'dap-node
  (add-hook 'typescript-mode-hook #'lsp-deferred)
  (dap-node-setup))

(provide 'pg-programming-typescript)
