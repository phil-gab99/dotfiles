;;; pg-guix.el -*- lexical-binding: t; -*-
;; Author: Philippe Gabriel

(when pg/is-guix-system
  (unless (fboundp 'guix)
    (autoload #'guix "guix" nil t))
  (with-eval-after-load 'general
    (pg/leader-keys
      "G" '(:ignore t :which-key "guix")
      "Gg" '(guix :which-key "guix")
      "Gs" '((lambda () (interactive) (dired "/run/current-system/profile/share/guile/site/3.0/")) :which-key "source"))))

(when pg/is-guix-system
  (unless (fboundp 'geiser-guile)
    (autoload #'geiser-guile "geiser" nil t))
  (unless (fboundp 'geiser-repl-clear-buffer)
    (autoload #'geiser-repl-clear-buffer "geiser" nil t))
  (if (boundp 'geiser-repl-mode-map)
      (define-key geiser-repl-mode-map (kbd "C-l") #'geiser-repl-clear-buffer)
    (with-eval-after-load 'geiser
      (define-key geiser-repl-mode-map (kbd "C-l") #'geiser-repl-clear-buffer)))
  (unless (fboundp 'corfu-mode)
    (autoload #'corfu-mode "corfu" nil t))
  (add-hook 'geiser-repl-mode-hook #'corfu-mode)

  (with-eval-after-load 'geiser-guile
    (add-to-list 'geiser-guile-load-path "~/Projects/guix")
    (pg/customize-set-variables
     `((geiser-guile-load-init-file . nil)
       (geiser-guile-manual-lookup-other-window . t))))

  (with-eval-after-load 'geiser
    (pg/customize-set-variables
     `((geiser-repl-company-p . nil)
       (geiser-repl-history-filename . ,(concat (getenv "XDG_CACHE_HOME") "/.geiser_history"))))))

(provide 'pg-guix)
