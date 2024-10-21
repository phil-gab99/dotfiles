;;; pg-guix.el -*- lexical-binding: t; -*-
;; Author: Philippe Gabriel

(when pg/is-guix-system
  (unless (fboundp 'guix)
    (autoload #'guix "guix" nil t))
  (unless (fboundp 'guix-devel-mode)
    (autoload #'guix-devel-mode "guix" nil t))
  (add-hook 'scheme-mode-hook #'guix-devel-mode)
  (with-eval-after-load 'general
    (pg/leader-keys
      "G" '(:ignore t :which-key "guix")
      "Gg" '(guix :which-key "guix"))))

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
  (add-hook 'geiser-repl-mode-hook #'(lambda ()
                                       (display-line-numbers-mode 0)))

  (with-eval-after-load 'geiser-guile
    (add-to-list 'geiser-guile-load-path (concat (plist-get pg/user :config) "/guix/current/share/guile/site/3.0"))
    (add-to-list 'geiser-guile-load-path (concat (plist-get pg/user :dotfiles) "/pg-channel"))
    (setopt geiser-guile-load-init-file nil
            geiser-guile-manual-lookup-other-window t))

  (with-eval-after-load 'geiser
    (setopt geiser-repl-company-p nil
            geiser-repl-history-filename (concat (plist-get pg/user :cache) "/.geiser_history"))))

(provide 'pg-guix)
