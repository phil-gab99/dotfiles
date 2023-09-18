;;; pg-programming-html.el -*- lexical-binding: t; -*-
;; Author: Philippe Gabriel

(add-hook 'mhtml-mode-hook #'lsp-deferred)

(straight-use-package 'web-mode)

(provide 'pg-programming-html)
