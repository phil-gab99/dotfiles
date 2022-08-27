;;; pg-programming-docker.el -*- lexical-binding: t; -*-
;; Author: Philippe Gabriel

(use-package docker
  :straight t
  :init
  (require 'docker))

(use-package dockerfile-mode
  :straight t
  :init
  (require 'dockerfile-mode)
  :after docker)

(provide 'pg-programming-docker)
