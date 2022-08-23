(use-package projectile
  :straight t
  :diminish projectile-mode
  :hook (lsp-mode . projectile-mode)
  :custom ((projectile-completion-system 'vertico))
  :init
  (setq projectile-keymap-prefix (kbd "C-c p"))
  (when (file-directory-p "~/Projects")
    (setq projectile-project-search-path '("~/Projects")))
  (setq projectile-switch-project-action #'projectile-dired))

(bind-keys*
 :map prog-mode-map
 ("C-p c" . projectile-run-project)
 ("C-p b" . projectile-compile-project))

(use-package magit
  :straight t
  :commands (magit-status magit-get-current-branch)
  :config
  (unbind-key "M-<tab>" 'magit-mode-map)
  (unbind-key "M-<tab>" 'magit-section-mode-map)
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package git-gutter
  :straight git-gutter-fringe
  :diminish git-gutter-mode
  :hook ((text-mode . git-gutter-mode)
         (prog-mode . git-gutter-mode))
  :custom
  (git-gutter-fr:side 'right-fringe)
  :config
  (unless pg/is-termux
    (require 'git-gutter-fringe)
    (set-face-foreground 'git-gutter-fr:added "LightGreen")
    (fringe-helper-define 'git-gutter-fr:added nil
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

    (set-face-foreground 'git-gutter-fr:modified "LightGoldenrod")
    (fringe-helper-define 'git-gutter-fr:modified nil
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

    (set-face-foreground 'git-gutter-fr:deleted "LightCoral")
    (fringe-helper-define 'git-gutter-fr:deleted nil
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

  ;; These characters are used in terminal mode
  (set-face-foreground 'git-gutter:added "LightGreen")
  (set-face-foreground 'git-gutter:modified "LightGoldenrod")
  (set-face-foreground 'git-gutter:deleted "LightCoral"))

(use-package forge
  :straight t
  :after magit)

(provide 'pg-project)
