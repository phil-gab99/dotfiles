;;; pg-programming-css.el -*- lexical-binding: t; -*-
;; Author: Philippe Gabriel

(with-eval-after-load 'css-mode
  (customize-set-variable 'css-indent-offset 2))

(unless (fboundp 'lsp-deferred)
  (autoload #'lsp-deferred "lsp-mode" nil t))
(dolist (mode '(css-mode-hook
                less-css-mode-hook
                scss-mode-hook))
  (add-hook mode #'lsp-deferred))

(provide 'pg-programming-css)
