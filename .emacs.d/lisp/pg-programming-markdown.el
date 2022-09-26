;;; pg-programming-markdown.el -*- lexical-binding: t; -*-
;; Author: Philippe Gabriel

(straight-use-package 'markdown-mode)
(with-eval-after-load 'markdown-mode
  (add-hook 'gfm-view-mode-hook (lambda ()
                                  (setq-local face-remapping-alist '((default (:height 1.5) variable-pitch)
                                                                     (markdown-code-face (:height 1.5) fixed-pitch))))))

(provide 'pg-programming-markdown)
