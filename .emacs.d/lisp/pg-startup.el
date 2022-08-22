(setq gc-cons-threshold (* 50 1000 1000)) ; Sets garbage collection threshold high enough

(server-start)

(unless (featurep 'straight)
  (defvar bootstrap-version)
  (let ((bootstrap-file
         (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
        (bootstrap-version 5))
    (unless (file-exists-p bootstrap-file)
      (with-current-buffer
          (url-retrieve-synchronously
           "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
           'silent 'inhibit-cookies)
        (goto-char (point-max))
        (eval-print-last-sexp)))
    (load bootstrap-file nil 'nomessage)))

(setq inhibit-startup-message t)               ; Disable startup message
(setq scroll-conservatively 1000)              ; Slow scrolling
(unless pg/is-termux
  (scroll-bar-mode 0)                          ; Disable visible scrollbar
  (tool-bar-mode 0)                            ; Disable toolbar
  (tooltip-mode 0))

(menu-bar-mode 0)                              ; Disable menu bar
(setq split-width-threshold 185)               ; Width for splitting
(global-set-key (kbd "M-<tab>") 'other-window) ; Bind alt tab to buffer switching

;; Set frame transparency
(unless (or pg/is-termux (not pg/exwm-enabled))
  (set-frame-parameter (selected-frame) 'alpha '(100 . 100))
  (add-to-list 'default-frame-alist '(alpha . (90 . 90)))
  (set-frame-parameter (selected-frame) 'fullscreen 'maximized)
  (add-to-list 'default-frame-alist '(fullscreen . maximized)))

(set-face-attribute 'default nil :font "JetBrains Mono" :weight 'light :height 120)
(set-face-attribute 'fixed-pitch nil :font "JetBrains Mono" :weight 'light)
(set-face-attribute 'variable-pitch nil :font "Iosevka Aile" :weight 'regular)

(set-face-attribute 'italic nil :slant 'italic)

(setq display-buffer-base-action
      '(display-buffer-reuse-mode-window
        display-buffer-reuse-window
        display-buffer-same-window))

;; If a popup does happen, don't resize windows to be equal-sized
(setq even-window-sizes nil)

(setq backup-directory-alist `(("." . ,(expand-file-name "tmp/backups/" user-emacs-directory)))
      auto-save-list-file-prefix (expand-file-name "tmp/auto-saves/sessions/" user-emacs-directory)
      auto-save-file-name-transforms `((".*" ,(expand-file-name "tmp/auto-saves/" user-emacs-directory) t)))
