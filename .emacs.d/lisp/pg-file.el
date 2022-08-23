(setq backup-directory-alist `(("." . ,(expand-file-name "tmp/backups/" user-emacs-directory)))
      auto-save-list-file-prefix (expand-file-name "tmp/auto-saves/sessions/" user-emacs-directory)
      auto-save-file-name-transforms `((".*" ,(expand-file-name "tmp/auto-saves/" user-emacs-directory) t)))

(setq vc-follow-symlinks t)

(use-package dired
  :straight nil
  :after evil-collection
  :commands (dired dired-jump)
  :bind (("C-x C-j" . dired-jump)) ; Open dired at current directory
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "h" 'dired-single-up-directory
    "l" 'dired-single-buffer)
  :custom ((dired-listing-switches "-agho --group-directories-first")))

(use-package dired-single
  :straight t
  :after dired
  :commands (dired dired-jump))

(unless pg/is-termux
  (use-package all-the-icons-dired
    :straight t
    :hook (dired-mode . all-the-icons-dired-mode)))

(use-package dired-hide-dotfiles
  :straight t
  :after evil-collection
  :hook (dired-mode . dired-hide-dotfiles-mode)
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "H" 'dired-hide-dotfiles-mode))

(unless pg/is-termux
  (use-package openwith
    :straight t
    :custom
    (large-file-warning-threshold nil)
    :config
    (setq openwith-associations
          (list
           (list
            (openwith-make-extension-regexp '("mpg" "mpeg" "mp4" "avi" "wmv" "mov" "flv" "ogm" "ogg" "mkv"))
            "mpv"
            '(file))
           (list
            (openwith-make-extension-regexp '("odt"))
            "libreoffice"
            '(file))))
    (openwith-mode 1)))

(provide 'pg-file)
