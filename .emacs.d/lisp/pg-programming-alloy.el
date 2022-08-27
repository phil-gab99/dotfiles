;;; pg-programming-alloy.el -*- lexical-binding: t; -*-
;; Author: Philippe Gabriel

(use-package alloy-mode
  :disabled
  :straight t
  :init
  (require 'alloy-mode)
  :hook
  (alloy-mode . (lambda ()
                  (setq indent-tabs-mode nil)))
  :custom
  (alloy-basic-offset 4))

(provide 'pg-programming-alloy)
