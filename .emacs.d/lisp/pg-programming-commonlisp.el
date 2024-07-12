;;; pg-programming-commonlisp.el -*- lexical-binding: t; -*-
;; Author: Philippe Gabriel

(straight-use-package 'sly)
(unless (fboundp 'sly)
  (autoload #'sly "sly" nil t))
(with-eval-after-load 'sly
  (setopt inferior-lisp-program "sbcl"))

(provide 'pg-programming-commonlisp)
