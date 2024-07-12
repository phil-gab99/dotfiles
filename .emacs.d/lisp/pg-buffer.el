;;; pg-buffer.el -*- lexical-binding: t; -*-
;; Author: Philippe Gabriel

(setq even-window-sizes nil
      display-buffer-base-action
      '(display-buffer-reuse-mode-window
        display-buffer-reuse-window
        display-buffer-same-window))

(straight-use-package 'bufler)
(unless (fboundp 'bufler)
  (autoload #'bufler "bufler" nil t))
(global-set-key (kbd "C-x C-b") #'bufler)
(with-eval-after-load 'bufler
  (unless (fboundp 'evil-collection-define-key)
    (autoload #'evil-collection-define-key "evil-collection"))
  (evil-collection-define-key 'normal 'bufler-list-mode-map
    (kbd "RET") #'bufler-list-buffer-switch
    (kbd "M-RET") #'bufler-list-buffer-peek
    "D" #'bufler-list-buffer-kill))

(require 'winner)
(with-eval-after-load 'winner
  (winner-mode)
  (with-eval-after-load 'general
    (pg/leader-keys
      "wu" '(winner-undo :which-key "undo config")
      "wr" '(winner-redo :which-key "redo config"))))

(require 'tab-bar)
(with-eval-after-load 'tab-bar
  (setopt tab-bar-show 1)
  (tab-bar-mode)
  (with-eval-after-load 'general
    (pg/leader-keys
      "wt" '(:ignore t :which-key "tabs")
      "wtt" '(tab-new :which-key "create")
      "wtw" '(tab-close :which-key "close")
      "wtr" '(tab-rename :which-key "name")
      "wts" '(tab-switch :which-key "switch")
      "wtu" '(tab-undo :which-key "undo"))))

(straight-use-package 'perspective)
(require 'perspective)
(with-eval-after-load 'perspective
  (setopt persp-suppress-no-prefix-key-warning t)
  (global-set-key (kbd "C-x k") #'persp-kill-buffer*)
  (unless (equal persp-mode t)
    (persp-mode))
  (with-eval-after-load 'general
    (pg/leader-keys
      "wp" '(:ignore t :which-key "persp")
      "wpc" '(persp-switch :which-key "create")
      "wps" '(hydra-perspective-switch/body :which-key "switch")
      "wpa" '(persp-add-buffer :which-key "add buf")
      "wpu" '(persp-set-buffer :which-key "set buf")
      "wpk" '(persp-kill :which-key "remove"))))

(provide 'pg-buffer)
