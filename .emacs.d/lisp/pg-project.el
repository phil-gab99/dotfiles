;;; pg-project.el -*- lexical-binding: t; -*-
;; Author: Philippe Gabriel

(straight-use-package 'projectile)
(require 'projectile)
(with-eval-after-load 'lsp-mode
  (add-hook 'lsp-mode-hook #'projectile-mode))
(with-eval-after-load 'projectile
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (when (file-directory-p "~/Projects")
    (customize-set-variable 'projectile-project-search-path "~/Projects"))
  (customize-set-variable 'projectile-switch-project-action #'projectile-dired)
  (if (boundp 'diminish)
      (diminish #'projectile-mode)
    (with-eval-after-load 'diminish
      (diminish #'projectile-mode)))
  (with-eval-after-load 'general
    (pg/leader-keys
      "p" '(:ignore t :which-key "project")
      "pf" '(projectile-find-file :which-key "find file")
      "ps" '(projectile-switch-project :which-key "switch project")
      "pr" '(projectile-run-project :which-key "run")
      "pc" '(projectile-compile-project :which-key "compile"))))

(straight-use-package 'magit)
(unless (fboundp 'magit-status)
  (autoload #'magit-status "magit-status" nil t))
(with-eval-after-load 'magit-status
  (unless (featurep 'magit)
    (require 'magit)))
(unless (fboundp 'magit-clone)
  (autoload #'magit-clone "magit-clone" nil t))
(with-eval-after-load 'magit-clone
  (unless (featurep 'magit)
    (require 'magit)))
(with-eval-after-load 'magit
  (customize-set-variable 'magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))
(with-eval-after-load 'general
  (pg/leader-keys
    "g" '(:ignore t :which-key "git")
    "gs" '(magit-status :which-key "status")
    "gc" '(magit-clone :which-key "clone")))

(straight-use-package 'git-gutter)
(unless (fboundp 'git-gutter-mode)
  (autoload #'git-gutter-mode "git-gutter" nil t))
(dolist (mode '(text-mode-hook
                prog-mode-hook))
  (add-hook mode #'git-gutter-mode))
(with-eval-after-load 'git-gutter
  (set-face-foreground 'git-gutter:added "LightGreen")
  (set-face-foreground 'git-gutter:modified "LightGoldenrod")
  (set-face-foreground 'git-gutter:deleted "LightCoral")
  (if (fboundp 'diminish)
      (diminish 'git-gutter-mode)
    (with-eval-after-load 'diminish
      (diminish 'git-gutter-mode))))

(straight-use-package 'forge)
(with-eval-after-load 'magit
  (require 'forge))
(with-eval-after-load 'forge
  (customize-set-variable 'forge-add-default-bindings nil))
(with-eval-after-load 'general
  (pg/leader-keys
    "gf" '(forge-pull :which-key "forge")))

(provide 'pg-project)
