;;; pg-programming-clojure.el -*- lexical-binding: t; -*-
;; Author: Philippe Gabriel

(straight-use-package 'clojure-mode)
(unless (fboundp 'lsp-deferred)
  (autoload #'lsp-deferred "lsp-mode" nil t))
(add-hook 'clojure-mode-hook #'lsp-deferred)

(provide 'pg-programming-clojure)
