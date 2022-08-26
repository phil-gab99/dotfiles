(straight-use-package 'projectile)
(unless (fboundp 'projectile-mode)
  (autoload #'projectile-mode "projectile" nil t))
(with-eval-after-load 'projectile
  (diminish 'projectile-mode)
  (add-hook 'lsp-mode-hook #'projectile-mode)
  (define-key projectile-mode-map (kbd "C-c p") #'projectile-command-map)
  (when (file-directory-p "~/Projects")
    (customize-set-variable 'projectile-project-search-path '("~/Projects")))
  (customize-set-variable 'projectile-switch-project-action #'projectile-dired))

(straight-use-package 'magit)
(unless (fboundp 'magit-status)
  (autoload #'magit-status "magit" nil t))
(unless (fboundp 'magit-get-current-branch)
  (autoload #'magit-get-current-branch "magit" nil t))
(with-eval-after-load 'magit
  (customize-set-variable 'magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(straight-use-package 'git-gutter)
(require 'git-gutter)
(unless (fboundp 'git-gutter-mode)
  (autoload #'git-gutter-mode "git-gutter" nil t))
(with-eval-after-load 'git-gutter
  (dolist (mode '(text-mode-hook
                  prog-mode-hook))
    (add-hook mode #'git-gutter-mode))
  (diminish 'git-gutter-mode)
  (set-face-foreground 'git-gutter:added "LightGreen")
  (set-face-foreground 'git-gutter:modified "LightGoldenrod")
  (set-face-foreground 'git-gutter:deleted "LightCoral"))

(straight-use-package 'forge)
(with-eval-after-load 'magit
  (require 'forge))

(provide 'pg-project)
