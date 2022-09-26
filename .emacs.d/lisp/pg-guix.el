;;; pg-guix.el -*- lexical-binding: t; -*-
;; Author: Philippe Gabriel

(unless pg/is-guix-system
  (unless (fboundp 'guix)
    (autoload #'guix "guix" nil t))
  (with-eval-after-load 'general
    (pg/leader-keys
      "G" '(:ignore t :which-key "Guix")
      "Gg" '(guix :which-key "Guix"))))

(unless pg/is-guix-system
  (unless (fboundp 'geiser-guile)
    (autoload #'geiser-guile "geiser" nil t))
  (unless (fboundp 'geiser-repl-clear-buffer)
    (autoload #'geiser-repl-clear-buffer "geiser" nil t))
  (if (boundp 'geiser-repl-mode-map)
      (define-key geiser-repl-mode-map (kbd "C-l") #'geiser-repl-clear-buffer)
    (with-eval-after-load 'geiser
      (define-key geiser-repl-mode-map (kbd "C-l") #'geiser-repl-clear-buffer)))
  (with-eval-after-load 'geiser
    (pg/customize-set-variables
     `((geiser-guile-load-init-file-p . t)
       (geiser-guile-manual-lookup-other-window-p . t)
       (geiser-guile-load-path . ("/run/current-system/profile/share/guile/3.0"))
       (geiser-repl-history-filename . ,(concat (getenv "XDG_CACHE_HOME") "/.geiser_history"))
       (geiser-guile-init-file . ,(concat (getenv "XDG_CONFIG_HOME") "/geiser/geiser-guile"))))))

(provide 'pg-guix)
