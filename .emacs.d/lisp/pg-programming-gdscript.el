;;; pg-programming-gdscript.el -*- lexical-binding: t; -*-
;; Author: Philippe Gabriel

(straight-use-package 'gdscript-mode)
(add-hook 'gdscript-mode-hook #'lsp-deferred)
(setopt lsp-gdscript-port 6008
        gdscript-use-tab-indents nil)

(provide 'pg-programming-gdscript)
