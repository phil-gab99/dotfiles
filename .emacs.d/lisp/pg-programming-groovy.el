;;; pg-programming-groovy.el -*- lexical-binding: t; -*-
;; Author: Philippe Gabriel

(use-package groovy-mode
  :straight '(groovy-emacs-modes :type git
                                 :host github
                                 :repo "Groovy-Emacs-Modes/groovy-emacs-modes")
  :init
  (require 'groovy-mode))

(provide 'pg-programming-groovy)
