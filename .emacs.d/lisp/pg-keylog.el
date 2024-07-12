;;; pg-keylog.el -*- lexical-binding: t; -*-
;; Author: Philippe Gabriel

(define-minor-mode pg/keycast-mode
  "Show current command and its key binding in the mode line.
Fix for use with doom-mode-line."
  :global t
  (interactive)
  (if pg/keycast-mode
      (add-hook 'pre-command-hook #'keycast--update t)
    (remove-hook 'pre-command-hook #'keycast--update)))

(straight-use-package 'keycast)
(require 'keycast)
(with-eval-after-load 'keycast
  (setopt keycast-mode-line-format "%2s%k%c%2s")
  (fset #'keycast-mode #'pg/keycast-mode)
  (keycast-mode)
  (add-to-list 'global-mode-string '("" keycast-mode-line)))

(provide 'pg-keylog)
