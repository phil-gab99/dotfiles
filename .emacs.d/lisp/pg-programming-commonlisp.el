;;; pg-programming-commonlisp.el -*- lexical-binding: t; -*-
;; Author: Philippe Gabriel

(use-package sly
  :straight t
  :init
  (require 'sly)
  :after lisp-mode
  :custom
  (inferior-lisp-program "sbcl"))

(provide 'pg-programming-commonlisp)
