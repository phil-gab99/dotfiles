;;; pg-notification.el -*- lexical-binding: t; -*-
;; Author: Philippe Gabriel

(use-package alert
  :straight t
  :init
  (require 'alert)
  :custom
  (alert-default-style 'notifications))

(provide 'pg-notification)
