;;; pg-guix.el -*- lexical-binding: t; -*-
;; Author: Philippe Gabriel

(use-package guix
  :straight nil
  :init
  (require 'guix)
  :commands guix)

(use-package geiser
  :straight nil
  :commands geiser-guile
  :custom
  (geiser-guile-load-init-file-p t)
  (geiser-guile-manual-lookup-other-window-p t)
  (geiser-guile-load-path '("/run/current-system/profile/share/guile/3.0"))
  (geiser-repl-history-filename (concat (getenv "XDG_CACHE_HOME") "/.geiser_history"))
  (geiser-guile-init-file (concat (getenv "XDG_CONFIG_HOME") "/geiser/geiser-guile"))
  :bind
  (:map geiser-repl-mode-map
        ("C-l" . geiser-repl-clear-buffer)))

(provide 'pg-guix)
