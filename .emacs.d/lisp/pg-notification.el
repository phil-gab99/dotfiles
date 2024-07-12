;;; pg-notification.el -*- lexical-binding: t; -*-
;; Author: Philippe Gabriel

(unless pg/is-windows
  (straight-use-package 'alert)
  (require 'alert)
  (with-eval-after-load 'alert
    (setopt alert-default-style 'notifications)))

(provide 'pg-notification)
