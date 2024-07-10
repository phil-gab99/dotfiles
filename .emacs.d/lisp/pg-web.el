;;; pg-web.el -*- lexical-binding: t; -*-
;; Author: Philippe Gabriel

(defun pg/eww-mode-setup ()
  (auto-fill-mode 0)
  (visual-line-mode 1)
  (setq-local face-remapping-alist '((default (:height 1.5) default))))
(add-hook 'eww-mode-hook #'pg/eww-mode-setup)

(provide 'pg-web)
