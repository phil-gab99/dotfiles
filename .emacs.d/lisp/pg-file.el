(setq backup-directory-alist `(("." . ,(expand-file-name "tmp/backups/" user-emacs-directory)))
      auto-save-list-file-prefix (expand-file-name "tmp/auto-saves/sessions/" user-emacs-directory)
      auto-save-file-name-transforms `((".*" ,(expand-file-name "tmp/auto-saves/" user-emacs-directory) t))
      vc-follow-symlinks t)

(use-package dired
  :straight nil
  :after evil
  :commands (dired dired-jump)
  :custom
  (dired-listing-switches "-agho --group-directories-first")
  :bind
  ("C-x C-j" . dired-jump)
  :config
  (evil-define-key 'normal 'dired-mode-map
    "h" 'dired-single-up-directory
    "l" 'dired-single-buffer))

(use-package dired-single
  :straight t
  :after dired)

(unless pg/is-termux
  (use-package all-the-icons-dired
    :straight t
    :hook
    (dired-mode . all-the-icons-dired-mode)))

(use-package dired-hide-dotfiles
  :straight t
  :after (dired evil)
  :hook
  (dired-mode-hook . dired-hide-dotfiles-mode)
  :config
  (evil-define-key 'normal 'dired-mode-map "H" 'dired-hide-dotfiles-mode))

(unless pg/is-termux
  (use-package openwith
    :disabled
    :straight t
    :custom
    (large-file-warning-threshold nil) 
    (openwith-associations `((,(openwith-make-extension-regexp '("mpg" "mpeg" "mp4" "avi" "wmv" "mov" "flv" "ogm" "ogg" "mkv"))
                              "mpv"
                              (file))
                             (,(openwith-make-extension-regexp '("odt"))
                              "libreoffice"
                              (file))))
    :config
    (openwith-mode 1)))

(provide 'pg-file)
