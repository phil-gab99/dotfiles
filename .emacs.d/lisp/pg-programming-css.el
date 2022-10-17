;;; pg-programming-css.el -*- lexical-binding: t; -*-
;; Author: Philippe Gabriel

(unless (fboundp 'lsp-deferred)
  (autoload #'lsp-deferred "lsp-mode" nil t))
(dolist (mode '(css-mode-hook
                less-css-mode-hook
                scss-mode-hook))
  (add-hook mode #'lsp-deferred))

(provide 'pg-programming-css)
