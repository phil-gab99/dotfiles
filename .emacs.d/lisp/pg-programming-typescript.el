;;; pg-programming-typescript.el -*- lexical-binding: t; -*-
;; Author: Philippe Gabriel

(straight-use-package 'typescript-mode)
(unless (fboundp 'typescript-mode)
  (autoload #'typescript-mode "typescript-mode" nil t))
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode))
(unless (fboundp 'lsp-deferred)
  (autoload #'lsp-deferred "lsp-mode" nil t))
(add-hook 'typescript-mode-hook #'lsp-deferred)
(dolist (fn #'(lsp-deferred
               rainbow-mode))
  (add-hook 'tsx-ts-mode-hook fn))
(setopt lsp-typescript-preferences-quote-style "single"
        lsp-typescript-references-code-lens-enabled t
        lsp-typescript-preferences-import-module-specifier "non-relative"
        lsp-typescript-update-imports-on-file-move-enabled "never")

(with-eval-after-load 'typescript-mode
  (setopt typescript-indent-level 2))

(with-eval-after-load 'typescript-mode
  (with-eval-after-load 'lsp-mode
    (require 'dap-node)))
(with-eval-after-load 'dap-node
  (dap-node-setup))

(provide 'pg-programming-typescript)
