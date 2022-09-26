;;; pg-programming-mips.el -*- lexical-binding: t; -*-
;; Author: Philippe Gabriel

(straight-use-package 'mips-mode)
(unless (fboundp 'mips-mode)
  (autoload #'mips-mode "mips-mode" nil t))
(add-to-list 'auto-mode-alist '("\\.asm$" . mips-mode))
(with-eval-after-load 'mips-mode
  (customize-set-variable 'mips-tab-width 4))

(provide 'pg-programming-mips)
