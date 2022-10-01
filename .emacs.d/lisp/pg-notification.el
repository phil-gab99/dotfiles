;;; pg-notification.el -*- lexical-binding: t; -*-
;; Author: Philippe Gabriel

(unless pg/is-windows-system
  (straight-use-package 'alert)
  (require 'alert)
  (with-eval-after-load 'alert
    (customize-set-variable 'alert-default-style 'notifications)))

(provide 'pg-notification)
