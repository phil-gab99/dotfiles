;;; pg-programming-erlang.el -*- lexical-binding: t; -*-
;; Author: Philippe Gabriel

(straight-use-package 'erlang)
(add-hook 'erlang-mode-hook #'lsp-deferred)

(provide 'pg-programming-erlang)
