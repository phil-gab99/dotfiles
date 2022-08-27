;;; pg-programming-css.el -*- lexical-binding: t; -*-
;; Author: Philippe Gabriel

(use-package lsp-css
  :straight nil
  :init
  (require 'lsp-css)
  :after lsp-mode
  :hook
  ((css-mode
    less-css-mode
    scss-mode) . lsp-deferred))

(provide 'pg-programming-css)
