;;; pg-programming-clojure.el -*- lexical-binding: t; -*-
;; Author: Philippe Gabriel

(straight-use-package 'clojure-ts-mode)
(add-to-list 'auto-mode-alist '("\\.clj\\'" . clojure-ts-mode))

(unless (fboundp 'lsp-deferred)
  (autoload #'lsp-deferred "lsp-mode" nil t))
(add-hook 'clojure-ts-mode-hook #'lsp-deferred)

(provide 'pg-programming-clojure)
