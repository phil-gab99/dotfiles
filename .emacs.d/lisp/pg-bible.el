;;; pg-bible.el -*- lexical-binding: t; -*-
;; Author: Philippe Gabriel

(use-package dtk
  :disabled
  :straight t
  :init
  (require 'dtk)
  :after evil
  :hook
  (dtk-mode . (lambda ()
                (setq-local face-remapping-alist '((default (:height 1.5) variable-pitch)))))
  :custom
  (dtk-module "KJV")
  (dtk-module-category "Biblical Texts")
  (dtk-word-wrap t)
  :config
  (evil-define-key 'normal 'dtk-mode-map
    (kbd "C-j") 'dtk-forward-verse
    (kbd "C-k") 'dtk-backward-verse
    (kbd "C-f") 'dtk-forward-chapter
    (kbd "C-b") 'dtk-backward-chapter
    "q" 'dtk-quit
    "c" 'dtk-clear-dtk-buffer
    "s" 'dtk-search))

(provide 'pg-bible)
