;;; pg-programming-typescript.el -*- lexical-binding: t; -*-
;; Author: Philippe Gabriel

(straight-use-package 'typescript-mode)
(unless (fboundp 'typescript-mode)
  (autoload #'typescript-mode "typescript-mode" nil t))
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode))
(unless (fboundp 'lsp-deferred)
  (autoload #'lsp-deferred "lsp-mode" nil t))
(add-hook 'typescript-mode-hook #'lsp-deferred)
(add-hook 'tsx-ts-mode-hook #'lsp-deferred)
(with-eval-after-load 'typescript-mode
  (customize-set-variable 'typescript-indent-level 2))

(with-eval-after-load 'typescript-mode
  (with-eval-after-load 'lsp-mode
    (require 'dap-node)))
(with-eval-after-load 'dap-node
  (dap-node-setup))

(provide 'pg-programming-typescript)
