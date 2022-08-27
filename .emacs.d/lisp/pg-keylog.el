(define-minor-mode pg/keycast-mode
  "Show current command and its key binding in the mode line (fix for use with
      doom-mode-line)."
  :global t
  (interactive)
  (if pg/keycast-mode
      (add-hook 'pre-command-hook #'keycast--update t)
    (remove-hook 'pre-command-hook #'keycast--update)))

(use-package keycast
  :straight t
  :custom
  (keycast-mode-line-format "%2s%k%c%2s")
  :config
  (fset #'keycast-mode #'pg/keycast-mode)
  (keycast-mode)
  (add-to-list 'global-mode-string '("" keycast-mode-line)))

(provide 'pg-keylog)
