;;; pg-programming-markdown.el -*- lexical-binding: t; -*-
;; Author: Philippe Gabriel

(straight-use-package 'markdown-mode)
(add-hook 'gfm-view-mode-hook (lambda ()
                                (setq-local face-remapping-alist '((default (:height 1.5) variable-pitch)
                                                                   (markdown-code-face (:height 1.0) org-code)))))
(with-eval-after-load 'markdown-mode
  (dolist (face '((markdown-header-face-1 . 1.9)
                  (markdown-header-face-2 . 1.7)
                  (markdown-header-face-3 . 1.5)
                  (markdown-header-face-4 . 1.3)
                  (markdown-header-face-5 . 1.1)
                  (markdown-header-face-6 . 1.05)))
    (set-face-attribute (car face) nil :weight 'regular :height (cdr face))))

(provide 'pg-programming-markdown)
