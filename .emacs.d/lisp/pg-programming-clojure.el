;;; pg-programming-clojure.el -*- lexical-binding: t; -*-
;; Author: Philippe Gabriel

(straight-use-package 'clojure-ts-mode)
(add-to-list 'auto-mode-alist '("\\.clj\\'" . clojure-ts-mode))

(provide 'pg-programming-clojure)
