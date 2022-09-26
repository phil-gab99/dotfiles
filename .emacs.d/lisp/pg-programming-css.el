;;; pg-programming-css.el -*- lexical-binding: t; -*-
;; Author: Philippe Gabriel

(with-eval-after-load 'lsp-mode
  (dolist (mode '(css-mode-hook
                  less-css-mode-hook
                  scss-mode-hook))
    (add-hook mode #'lsp-deferred)))

(provide 'pg-programming-css)
