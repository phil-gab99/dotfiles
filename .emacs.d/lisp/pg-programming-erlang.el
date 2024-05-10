;;; pg-programming-erlang.el -*- lexical-binding: t; -*-
;; Author: Philippe Gabriel

(straight-use-package 'erlang)

(add-hook 'erlang-mode-hook #'lsp-deferred)
;; (add-to-list 'auto-mode-alist '("\\.erl\\'" . erlang-ts-mode))

(provide 'pg-programming-erlang)
