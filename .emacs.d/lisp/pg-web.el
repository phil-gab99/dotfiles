;;; pg-web.el -*- lexical-binding: t; -*-
;; Author: Philippe Gabriel

(defun pg/eww-mode-setup ()
  (auto-fill-mode 0)
  (visual-line-mode 1)
  (setq-local face-remapping-alist '((variable-pitch (:height 2.0) variable-pitch)
                                     (fixed-pitch (:height 2.0) fixed-pitch)
                                     (default (:height 2.0) default))))

(require 'eww)
(with-eval-after-load 'eww
  (add-hook 'eww-mode-hook #'pg/eww-mode-setup))

(provide 'pg-web)