;;; pg-file.el -*- lexical-binding: t; -*-
;; Author: Philippe Gabriel

(pg/customize-set-variables
 `((backup-directory-alist . (("." . ,(expand-file-name "tmp/backups/" user-emacs-directory))))
   (enable-remote-dir-locals . t)
   (auto-save-file-name-transforms . ((".*" ,(expand-file-name "tmp/auto-saves/" user-emacs-directory) t)))
   (safe-local-variable-values . ((projectile-project-compilation-cmd . "./mvnw clean package")))
   (vc-follow-symlinks . t)))

(unless (fboundp 'dired)
  (autoload #'dired "dired" nil t))
(unless (fboundp 'dired-jump)
  (autoload #'dired-jump "dired" nil t))
(global-set-key (kbd "C-x C-j") #'dired-jump)
(with-eval-after-load 'dired
  (customize-set-variable 'dired-listing-switches "-Alh --group-directories-first"))

(straight-use-package 'dired-single)
(with-eval-after-load 'dired
  (require 'dired-single))
(with-eval-after-load 'dired-single
  (if (boundp 'dired-mode-map)
      (progn
        (define-key dired-mode-map [remap dired-find-file] 'dired-single-buffer)
        (define-key dired-mode-map [remap dired-mouse-find-file-other-window] 'dired-single-buffer-mouse)
        (define-key dired-mode-map [remap dired-up-directory] 'dired-single-up-directory)))
  (unless (fboundp 'evil-collection-define-key)
    (autoload #'evil-collection-define-key "evil-collection"))
  (evil-collection-define-key 'normal 'dired-mode-map
    "h" #'dired-single-up-directory
    "l" #'dired-single-buffer))

(straight-use-package 'nerd-icons-dired)
(unless pg/is-termux
  (unless (fboundp 'nerd-icons-dired-mode)
    (autoload #'nerd-icons-dired-mode "nerd-icons-dired" nil t))
  (add-hook 'dired-mode-hook #'nerd-icons-dired-mode))

;; (straight-use-package 'all-the-icons-dired)
;; (unless pg/is-termux
;;   (unless (fboundp 'all-the-icons-dired-mode)
;;     (autoload #'all-the-icons-dired-mode "all-the-icons-dired" nil t))
;;   (add-hook 'dired-mode-hook #'all-the-icons-dired-mode))

(straight-use-package 'dired-hide-dotfiles)
(with-eval-after-load 'dired
  (unless (fboundp 'dired-hide-dotfiles-mode)
    (autoload #'dired-hide-dotfiles-mode "dired-hide-dotfiles" nil t))
  (add-hook 'dired-mode-hook #'dired-hide-dotfiles-mode)
  (with-eval-after-load 'evil-collection
    (evil-collection-define-key 'normal 'dired-mode-map
      "H" #'dired-hide-dotfiles-mode)))

(unless pg/is-termux 
  (straight-use-package 'openwith)
  (when (require 'openwith nil 'noerror)
    (pg/customize-set-variables
     `((large-file-warning-threshold . nil)
       (openwith-associations . ((,(openwith-make-extension-regexp '("mpg"
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
                                 (,(openwith-make-extension-regexp '("odt"
                                                                     "ods"))
                                  "libreoffice"
                                  (file))
                                 (,(openwith-make-extension-regexp '("xopp"))
                                  "xournalpp"
                                  (file))))))
    (openwith-mode 1)))

(straight-use-package '(subed :type git
                              :host github
                              :repo "sachac/subed"
                              :files ("subed/*.el")))

(with-eval-after-load 'subed
  (dolist (mode #'(save-place-local-mode
                   turn-on-auto-fill))
    (add-hook 'subed-mode-hook mode)))

(provide 'pg-file)
