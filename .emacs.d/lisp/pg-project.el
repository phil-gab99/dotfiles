;;; pg-project.el -*- lexical-binding: t; -*-
;; Author: Philippe Gabriel

(use-package projectile
  :straight t
  :init
  (require 'projectile)
  :after diminish
  :diminish projectile-mode
  :hook
  (lsp-mode . projectile-mode)
  :custom
  (projectile-switch-project-action #'projectile-dired)
  :bind
  (:map projectile-mode-map
	("C-c p" . projectile-command-map))
  :config
  (when (file-directory-p "~/Projects")
    (customize-set-variable 'projectile-project-search-path '("~/Projects"))))

(use-package magit
  :straight t
  :init
  (require 'magit)
  :commands (magit-status magit-get-current-branch)
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package git-gutter
  :straight t
  :init
  (require 'git-gutter)
  :after diminish
  :diminish git-gutter-mode
  :hook
  ((text-mode-hook
    prog-mode-hook) . git-gutter-mode)
  :config
  (set-face-foreground 'git-gutter:added "LightGreen")
  (set-face-foreground 'git-gutter:modified "LightGoldenrod")
  (set-face-foreground 'git-gutter:deleted "LightCoral"))

(use-package forge
  :straight t
  :init
  (require 'forge)
  :after magit
  :custom
  (forge-add-default-bindings nil))

(provide 'pg-project)
