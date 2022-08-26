(setq backup-directory-alist `(("." . ,(expand-file-name "tmp/backups/" user-emacs-directory)))
      auto-save-list-file-prefix (expand-file-name "tmp/auto-saves/sessions/" user-emacs-directory)
      auto-save-file-name-transforms `((".*" ,(expand-file-name "tmp/auto-saves/" user-emacs-directory) t))
      vc-follow-symlinks t)

(unless (fboundp 'dired-jump)
  (autoload #'dired-jump "dired" nil t))
(unless (fboundp 'dired)
  (autoload #'dired "dired" nil t))
(with-eval-after-load 'dired
  (global-set-key (kbd "C-x C-j") #'dired-jump)
  (evil-collection-define-key 'normal 'dired-mode-map "h" 'dired-single-up-directory "l" 'dired-single-buffer)
  (customize-set-variable 'dired-listing-switches "-agho --group-directories-first"))

(straight-use-package 'dired-single)
(with-eval-after-load 'dired
  (require 'dired-single))

(straight-use-package 'all-the-icons-dired)
(unless pg/is-termux
  (require 'all-the-icons-dired)
  (with-eval-after-load 'all-the-icons-dired
    (add-hook 'dired-mode-hook #'all-the-icons-dired-mode)))

(straight-use-package 'dired-hide-dotfiles)
(with-eval-after-load 'evil-collection
  (unless (fboundp 'dired-hide-dotfiles-mode)
    (autoload #'dired-hide-dotfiles-mode "dired-hide-dotfiles" nil t))
  (with-eval-after-load 'dired-hide-dotfiles
    (add-hook 'dired-mode-hook #'dired-hide-dotfiles-mode)
    (evil-collection-define-key 'normal 'dired-mode-map "H" 'dired-hide-dotfiles-mode)))

(straight-use-package 'openwith)
(unless pg/is-termux
  (require 'openwith)
  (with-eval-after-load 'openwith
    (customize-set-variable 'large-file-warning-threshold nil)
    (customize-set-variable 'openwith-associations
                            `((,(openwith-make-extension-regexp '("mpg" "mpeg" "mp4" "avi" "wmv" "mov" "flv" "ogm" "ogg" "mkv"))
                               "mpv"
                               (file))
                              (,(openwith-make-extension-regexp '("odt"))
                               "libreoffice"
                               (file))))
    (openwith-mode 1)))

(provide 'pg-file)
