;;; pg-programming-docker.el -*- lexical-binding: t; -*-
;; Author: Philippe Gabriel

(straight-use-package 'docker)
(add-to-list 'auto-mode-alist
             (cons (concat "[/\\]"
                           "\\(?:Containerfile\\|Dockerfile\\)"
                           "\\(?:\\.[^/\\]*\\)?\\'")
                   'dockerfile-ts-mode))
(add-hook 'dockerfile-ts-mode-hook #'lsp-deferred)

(provide 'pg-programming-docker)
