;;; pg-programming-gdscript.el -*- lexical-binding: t; -*-
;; Author: Philippe Gabriel

(straight-use-package 'gdscript-mode)
(pg/customize-set-variables
 '((lsp-gdscript-port . 6008)
   (gdscript-use-tab-indents . nil)))

(provide 'pg-programming-gdscript)
