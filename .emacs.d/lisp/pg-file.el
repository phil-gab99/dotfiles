;;; pg-file.el -*- lexical-binding: t; -*-
;; Author: Philippe Gabriel

(setq backup-directory-alist `(("." . ,(expand-file-name "tmp/backups/" user-emacs-directory)))
      auto-save-list-file-prefix (expand-file-name "tmp/auto-saves/sessions/" user-emacs-directory)
      auto-save-file-name-transforms `((".*" ,(expand-file-name "tmp/auto-saves/" user-emacs-directory) t))
      vc-follow-symlinks t)

(use-package dired
  :straight nil
  :init
  (require 'dired)
  :after evil-collection
  :commands (dired dired-jump)
  :custom
  (dired-listing-switches "-agho --group-directories-first")
  :bind
  ("C-x C-j" . dired-jump))

(use-package dired-single
  :straight t
  :init
  (require 'dired-single)
  :after dired
  :commands (dired dired-jump)
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "h" 'dired-single-up-directory
    "l" 'dired-single-buffer))

(unless pg/is-termux
  (use-package all-the-icons-dired
    :straight t
    :init
    (require 'all-the-icons-dired)
    :hook
    (dired-mode . all-the-icons-dired-mode)))

(use-package dired-hide-dotfiles
  :straight t
  :init
  (require 'dired-hide-dotfiles)
  :after (dired evil-collection)
  :hook
  (dired-mode-hook . dired-hide-dotfiles-mode)
  :config
  (evil-collection-define-key 'normal 'dired-mode-map "H" 'dired-hide-dotfiles-mode))

(unless pg/is-termux 
  (straight-use-package 'openwith)
  (when (require 'openwith nil 'noerror)
    (customize-set-variable 'large-file-warning-threshold nil)
    (customize-set-variable 'openwith-associations `((,(openwith-make-extension-regexp '("mpg"
                                                                                         "mpeg"
                                                                                         "mp4"
                                                                                         "avi"
                                                                                         "wmv"
                                                                                         "mov"
                                                                                         "flv"
                                                                                         "ogm"
                                                                                         "ogg"
                                                                                         "mkv"))
                                                      "mpv"
                                                      (file))
                                                     (,(openwith-make-extension-regexp '("odt"))
                                                      "libreoffice"
                                                      (file))))
    (openwith-mode 1)))

(provide 'pg-file)
