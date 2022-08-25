(straight-use-package 'projectile)
(unless (fboundp 'projectile-mode)
  (autoload #'projectile-mode "projectile" nil t))
(with-eval-after-load 'projectile
  (diminish 'projectile-mode)
  (add-hook 'lsp-mode-hook #'projectile-mode)
  (bind-key "C-c p" #'projectile-command-map projectile-mode-map)
  (when (file-directory-p "~/Projects")
    (customize-set-variable 'projectile-project-search-path '("~/Projects")))
  (customize-set-variable 'projectile-switch-project-action #'projectile-dired)

(unless (fboundp 'magit-status)
  (autoload #'magit-status "magit" nil t))
(unless (fboundp 'magit-get-current-branch)
  (autoload #'magit-get-current-branch "magit" nil t))
(with-eval-after-load 'magit
  (customize-set-variable 'magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(require 'git-gutter)
(unless (fboundp 'git-gutter-mode)
  (autoload #'git-gutter-mode "git-gutter" nil t))
(with-eval-after-load 'git-gutter
  (dolist (mode '(text-mode-hook
                  prog-mode-hook))
    (add-hook mode #'git-gutter-mode))
  (diminish 'git-gutter-mode)
  (set-face-foreground 'git-gutter:added-sign "LightGreen")
  (fringe-helper-define 'git-gutter:added-sign nil
    "XXXXXXXXXX"
    "XXXXXXXXXX"
    "XXXXXXXXXX"
    ".........."
    ".........."
    "XXXXXXXXXX"
    "XXXXXXXXXX"
    "XXXXXXXXXX"
    ".........."
    ".........."
    "XXXXXXXXXX"
    "XXXXXXXXXX"
    "XXXXXXXXXX")

  (set-face-foreground 'git-gutter:modified-sign "LightGoldenrod")
  (fringe-helper-define 'git-gutter:modified-sign nil
    "XXXXXXXXXX"
    "XXXXXXXXXX"
    "XXXXXXXXXX"
    ".........."
    ".........."
    "XXXXXXXXXX"
    "XXXXXXXXXX"
    "XXXXXXXXXX"
    ".........."
    ".........."
    "XXXXXXXXXX"
    "XXXXXXXXXX"
    "XXXXXXXXXX")

  (set-face-foreground 'git-gutter:deleted-sign "LightCoral")
  (fringe-helper-define 'git-gutter:deleted-sign nil
    "XXXXXXXXXX"
    "XXXXXXXXXX"
    "XXXXXXXXXX"
    ".........."
    ".........."
    "XXXXXXXXXX"
    "XXXXXXXXXX"
    "XXXXXXXXXX"
    ".........."
    ".........."
    "XXXXXXXXXX"
    "XXXXXXXXXX"
    "XXXXXXXXXX"))

(with-eval-after-load 'magit
  (require 'forge))

(provide 'pg-project)
