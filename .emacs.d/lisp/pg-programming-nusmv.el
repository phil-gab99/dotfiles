(use-package nusmv-mode
  :straight nil
  :disabled ;; Need NuSMV binary
  :load-path "~/.emacs.d/extrapkgs/nusmv-mode"
  :mode "\\.smv\\'"
  :bind*
  (:map nusmv-mode-map
        ("C-c C-c" . nusmv-run))
  (:map nusmv-m4-mode-map
        ("C-c C-c" . nusmv-run))
  :custom
  (nusmv-indent 4)
  :config
  (menu-bar-mode 0)
  (add-hook 'nusmv-mode-hook (lambda () (run-hooks 'prog-mode-hook)))
  (put 'nusmv-mode 'derived-mode-parent 'prog-mode))

(provide 'pg-programming-nusmv)
