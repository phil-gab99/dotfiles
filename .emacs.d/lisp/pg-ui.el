;;; pg-ui.el -*- lexical-binding: t; -*-
;; Author: Philippe Gabriel

(setq inhibit-startup-message t  ; Disable startup message
      scroll-conservatively 1000 ; Slow scrolling
      split-width-threshold 185) ; Width for splitting
(unless pg/is-termux
  (scroll-bar-mode 0)            ; Disable visible scrollbar
  (tool-bar-mode 0)              ; Disable toolbar
  (tooltip-mode 0))
(menu-bar-mode 0)                ; Disable menu bar

;; Set frame transparency
(unless (or pg/is-termux (not pg/exwm-enabled))
  (set-frame-parameter (selected-frame) 'alpha '(90 . 90))
  (add-to-list 'default-frame-alist '(alpha . (90 . 90)))
  (set-frame-parameter (selected-frame) 'fullscreen 'maximized)
  (add-to-list 'default-frame-alist '(fullscreen . maximized)))

(use-package diminish
  :straight t
  :init
  (require 'diminish))

(use-package all-the-icons
  :straight t
  :init
  (require 'all-the-icons))

(use-package doom-modeline
  :straight t
  :init
  (require 'doom-modeline)
  :custom
  (doom-modeline-height 15)
  (doom-modeline-modal-icon nil)
  (doom-modeline-enable-word-count t)
  (doom-modeline-indent-info t)
  (doom-modeline-buffer-file-name-style 'truncate-except-project)
  (doom-modeline-mu4e t)
  :config
  (doom-modeline-mode 1))

(use-package autothemer
  :straight t
  :init
  (require 'autothemer)
  :config
  (load-theme 'onedark-variant t))

(defun pg/dashboard-setup-startup-hook ()
  "Setup post initialization hooks."
  (add-hook 'after-init-hook #'(lambda ()
                                 ;; Display useful lists of items
                                 (dashboard-insert-startupify-lists)))
  (add-hook 'emacs-startup-hook #'(lambda ()
                                    (switch-to-buffer dashboard-buffer-name)
                                    (goto-char (point-min))
                                    (redisplay)
                                    (run-hooks 'dashboard-after-initialize-hook))))

(defun pg/display-startup-time ()
  "Displays some startip statistics."
  (let ((package-count 0) (time (float-time (time-subtract after-init-time before-init-time))))
    (when (boundp 'straight--profile-cache)
      (setq package-count (+ (hash-table-count straight--profile-cache) package-count)))
    (if (zerop package-count)
        (format "Emacs started in %.2f" time)
      (format "%d packages loaded in %.2f seconds with %d garbage collections" package-count time gcs-done))))

(use-package dashboard
  :straight t
  :init
  (fset #'dashboard-setup-startup-hook #'pg/dashboard-setup-startup-hook)
  (require 'dashboard)
  :after projectile
  :custom
  (dashboard-set-file-icons t)
  (dashboard-items '((recents . 5)
                     (projects . 10)
                     (agenda . 5)))
  (dashboard-page-separator "\n\f\n")
  (dashboard-init-info #'pg/display-startup-time)
  :config
  (pg/dashboard-setup-startup-hook))

(use-package page-break-lines
  :straight t
  :init
  (require 'page-break-lines))

(provide 'pg-ui)
