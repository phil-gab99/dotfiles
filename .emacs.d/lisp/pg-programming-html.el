;;; pg-programming-html.el -*- lexical-binding: t; -*-
;; Author: Philippe Gabriel

(add-hook 'mhtml-mode-hook #'lsp-deferred)

(straight-use-package 'web-mode)
(setopt web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2
        web-mode-markup-indent-offset 2
        web-mode-markup-comment-indent-offset 2
        web-mode-enable-curly-brace-indentation t)

(provide 'pg-programming-html)
