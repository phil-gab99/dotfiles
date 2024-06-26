;;; pg-ui.el -*- lexical-binding: t; -*-
;; Author: Philippe Gabriel

(load-theme 'pg-onedark-variant t) ;; Load onedark theme
;; (load-theme 'pg-onelight-variant t) ;; Load onelight theme
(setq inhibit-startup-message t    ;; Disable startup message
      scroll-conservatively 1000   ;; Slow scrolling
      split-width-threshold 185)   ;; Width for splitting
(unless pg/is-termux
  (scroll-bar-mode 0)              ;; Disable visible scrollbar
  (tool-bar-mode 0)                ;; Disable toolbar
  (tooltip-mode 0))
(menu-bar-mode 0)                  ;; Disable menu bar

;; Set frame transparency
(unless (or pg/is-termux (not pg/exwm-enabled))
  (set-frame-parameter nil 'alpha '90)
  (add-to-list 'default-frame-alist '(alpha . 90))
  (set-frame-parameter nil 'fullscreen 'maximized)
  (add-to-list 'default-frame-alist '(fullscreen . maximized)))

;; Set fullscreen
(if pg/is-windows
   (add-to-list 'default-frame-alist '(fullscreen . maximized)))

(straight-use-package 'diminish)
(require 'diminish)

(defun pg/diminish-minor-modes ()
  "Diminishes some minor modes which affect org tree slides"
  (unless (fboundp 'diminish)
    (autoload #'diminish "diminish" nil t))
  (dolist (mode #'(auto-revert-mode
                   buffer-face-mode
                   visual-line-mode))
    (diminish mode)))

(straight-use-package 'nerd-icons)
(require 'nerd-icons)

;; (straight-use-package 'all-the-icons)
;; (require 'all-the-icons)

(straight-use-package 'doom-modeline)
(require 'doom-modeline)
(with-eval-after-load 'doom-modeline
  (pg/customize-set-variables
   '((doom-modeline-height . 17)
     (doom-modeline-modal-icon . nil)
     (doom-modeline-enable-word-count . t)
     (doom-modeline-indent-info . t)
     (doom-modeline-buffer-file-name-style . truncate-except-project)
     (doom-modeline-mu4e . t)))
  (doom-modeline-mode 1))

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

(straight-use-package 'dashboard)
(with-eval-after-load 'projectile
  (require 'dashboard)
  ;; (with-eval-after-load 'nerd-icons
  ;;   (require 'dashboard))
  (fset #'dashboard-setup-startup-hook #'pg/dashboard-setup-startup-hook))
(pg/customize-set-variables
 `((dashboard-items . ((recents . 5)
                       (projects . 5)
                       (agenda . 5)))
   (dashboard-set-heading-icons . t)
   (dashboard-set-file-icons . t)
   (dashboard-display-icons-p . t)
   ;; (dashboard-icon-type . all-the-icons)
   ;; (dashboard-heading-icons . ((recents . "history")
   ;;                             (bookmarks . "bookmark")
   ;;                             (agenda . "calendar")
   ;;                             (projects . "rocket")
   ;;                             (registers . "database")))
   (dashboard-match-agenda-entry . "task")
   (dashboard-page-separator . "\n\f\n")
   (dashboard-init-info . ,#'pg/display-startup-time)))
(with-eval-after-load 'dashboard
  (pg/dashboard-setup-startup-hook))

(straight-use-package 'page-break-lines)
(require 'page-break-lines)

(provide 'pg-ui)
