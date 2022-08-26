(setq even-window-sizes nil
      display-buffer-base-action
      '(display-buffer-reuse-mode-window
        display-buffer-reuse-window
        display-buffer-same-window))

(straight-use-package 'bufler)
(unless (fboundp 'bufler)
  (autoload #'bufler "bufler" nil t))
(with-eval-after-load 'evil-collection
  (with-eval-after-load 'bufler
    (evil-collection-define-key 'normal 'bufler-list-mode-map
      (kbd "RET")   'bufler-list-buffer-switch
      (kbd "M-RET") 'bufler-list-buffer-peek
      "D"           'bufler-list-buffer-kill)
    (global-set-key (kbd "C-x C-b") #'bufler)
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
            (auto-mode)))))

(require 'winner)
(with-eval-after-load 'winner
  (winner-mode))

(require 'tab-bar)
(with-eval-after-load 'tab-bar
  (customize-set-variable 'tab-bar-show 1)
  (tab-bar-mode))

(straight-use-package 'perspective)
(unless (fboundp 'persp-kill-buffer*)
  (autoload #'persp-kill-buffer* "perspective" nil t))
(with-eval-after-load 'perspective
  (global-set-key (kbd "C-x k") #'persp-kill-buffer*)
  (customize-set-variable 'persp-suppress-no-prefix-key-warning t)
  (unless (equal persp-mode t) (persp-mode)))

(provide 'pg-buffer)
