;;; pg-project.el -*- lexical-binding: t; -*-
;; Author: Philippe Gabriel

(straight-use-package 'projectile)
(require 'projectile)
(add-hook 'lsp-mode-hook #'projectile-mode)
(with-eval-after-load 'projectile
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (when (file-directory-p "~/Projects")
    (setopt projectile-project-search-path '("~/Projects")))
  (setopt projectile-switch-project-action #'projectile-dired)
  (unless (fboundp 'diminish)
    (autoload #'diminish "diminish" nil t))
  (diminish #'projectile-mode)

  (projectile-register-project-type 'npm '("angular.json")
                                    :project-file "package.json"
                                    :compile "npm i && npm run build"
                                    :run "npm start"
                                    :test "npm test"
                                    :test-suffix ".spec")

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
(unless (fboundp 'magit-clone)
  (autoload #'magit-clone "magit-clone" nil t))
(with-eval-after-load 'magit
  (if pg/is-windows
      (setenv "SSH_ASKPASS" "git-gui--askpass"))
  (setopt magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))
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
  (unless (fboundp 'diminish)
    (autoload #'diminish "diminish" nil t))
  (diminish #'git-gutter-mode))

(unless pg/is-windows
  (straight-use-package 'forge)
  (with-eval-after-load 'magit
    (require 'forge))
  (with-eval-after-load 'forge
    (setopt forge-add-default-bindings nil))
  (with-eval-after-load 'general
    (pg/leader-keys
      "gf" '(forge-pull :which-key "forge"))))

(provide 'pg-project)
