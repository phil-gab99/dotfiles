;;; pg-programming-mips.el -*- lexical-binding: t; -*-
;; Author: Philippe Gabriel

(use-package mips-mode
  :straight t
  :init
  (require 'mips-mode)
  :mode "\\.asm$"
  :custom
  (mips-tab-width 4))

(provide 'pg-programming-mips)
