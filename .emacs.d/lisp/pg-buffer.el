;;; pg-buffer.el -*- lexical-binding: t; -*-
;; Author: Philippe Gabriel

(setq even-window-sizes nil
      display-buffer-base-action
      '(display-buffer-reuse-mode-window
        display-buffer-reuse-window
        display-buffer-same-window))

(use-package bufler
  :straight t
  :init
  (require 'bufler)
  :after evil
  :commands bufler
  :bind
  ("C-x C-b" . bufler)
  :config
  (evil-define-key 'normal 'bufler-list-mode-map
    (kbd "RET")   'bufler-list-buffer-switch
    (kbd "M-RET") 'bufler-list-buffer-peek
    "D"           'bufler-list-buffer-kill)
  (setf bufler-groups
        (bufler-defgroups

          ;; Subgroup collecting all named workspaces.
          (group (auto-workspace))

          ;; Subgroup collecting buffers in a projectile project.
          (group (auto-projectile))

          ;; Grouping browser windows
          (group
           (group-or "Browsers"
                     (name-match "Firefox" (rx bos "firefox"))))

          (group
           (group-or "Chat"
                     (name-match "Discord" (rx bos "discord"))
                     (mode-match "Slack" (rx bos "slack-"))))

          (group
           ;; Subgroup collecting all `help-mode' and `info-mode' buffers.
           (group-or "Help/Info"
                     (mode-match "*Help*" (rx bos (or "help-" "helpful-")))
                     (mode-match "*Info*" (rx bos "info-"))))

          (group
           ;; Subgroup collecting all special buffers (i.e. ones that are not
           ;; file-backed), except `magit-status-mode' buffers (which are allowed to fall
           ;; through to other groups, so they end up grouped with their project buffers).
           (group-and "*Special*"
                      (name-match "**Special**"
                                  (rx bos "*" (or "Messages" "Warnings" "scratch" "Backtrace" "Pinentry") "*"))
                      (lambda (buffer)
                        (unless (or (funcall (mode-match "Magit" (rx bos "magit-status"))
                                             buffer)
                                    (funcall (mode-match "Dired" (rx bos "dired"))
                                             buffer)
                                    (funcall (auto-file) buffer))
                          "*Special*"))))

          ;; Group remaining buffers by major mode.
          (auto-mode))))

(use-package winner
  :straight nil
  :init
  (require 'winner)
  :config
  (winner-mode))

(use-package tab-bar
  :straight nil
  :init
  (require 'tab-bar)
  :custom
  (tab-bar-show 1)
  :config
  (tab-bar-mode))

(use-package perspective
  :straight t
  :init
  (require 'perspective)
  :custom
  (persp-suppress-no-prefix-key-warning t)
  :bind
  ("C-x k" . persp-kill-buffer*)
  :config
  (unless (equal persp-mode t)
    (persp-mode)))

(provide 'pg-buffer)
