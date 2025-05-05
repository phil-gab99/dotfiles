;;; pg-bible.el -*- lexical-binding: t; -*-
;; Author: Philippe Gabriel

(straight-use-package 'dtk)
(unless (fboundp 'dtk)
  (autoload #'dtk "dtk" nil t))
(add-hook 'dtk-mode-hook #'(lambda ()
                             (setq-local face-remapping-alist
                                         '((default (:height 1.5) default)))))
(with-eval-after-load 'dtk
  (setopt dtk-module "KJV"
          dtk-module-category "Biblical Texts"
          dtk-word-wrap t)
  (with-eval-after-load 'evil
    (evil-define-key 'normal dtk-mode-map
      (kbd "C-j") #'dtk-forward-verse
      (kbd "C-k") #'dtk-backward-verse
      (kbd "C-f") #'dtk-forward-chapter
      (kbd "C-b") #'dtk-backward-chapter
      "q" #'dtk-quit
      "c" #'dtk-clear-dtk-buffer
      "s" #'dtk-search)))

(provide 'pg-bible)
