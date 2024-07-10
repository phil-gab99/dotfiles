;;; pg-programming-i3config.el -*- lexical-binding: t; -*-
;; Author: Philippe Gabriel

(straight-use-package 'i3wm-config-mode)
(unless (fboundp 'i3wm-config-mode)
  (autoload #'i3wm-config-mode "i3wm-config-mode" nil t))

(provide 'pg-programming-i3config)
