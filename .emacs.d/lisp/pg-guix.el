;;; pg-guix.el -*- lexical-binding: t; -*-
;; Author: Philippe Gabriel

(use-package guix
  :straight nil
  :init
  (require 'guix)
  :commands guix)

(use-package geiser
  :straight nil
  :init
  (require 'geiser)
  :after guix)
  ;; :bind
  ;; (:map geiser-repl-mode-map
  ;;       ("C-l" . geiser-repl-clear-buffer)))

(provide 'pg-guix)
