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

(require 'diminish)

(require 'all-the-icons)

(require 'doom-modeline)
(with-eval-after-load 'doom-modeline
  (doom-modeline-mode 1)
  (customize-set-variable 'doom-modeline-height 15)
  (customize-set-variable 'doom-modeline-modal-icon nil)
  (customize-set-variable 'doom-modeline-enable-word-count t)
  (customize-set-variable 'doom-modeline-indent-info t)
  (customize-set-variable 'doom-modeline-buffer-file-name-style 'truncate-except-project)
  (customize-set-variable 'doom-modeline-mu4e t))

(straight-use-package 'autothemer)
(require 'autothemer)
(with-eval-after-load 'autothemer
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

(straight-use-package 'dashboard)
(require 'dashboard)
(with-eval-after-load 'dashboard
  (customize-set-variable 'dashboard-set-file-icons t)
  (customize-set-variable 'dashboard-items '((recents . 10)
                                             (projects . 10)
                                             (agenda . 5)))
  (customize-set-variable 'dashboard-page-separator "\n\f\n")
  (customize-set-variable 'dashboard-init-info #'pg/display-startup-time)
  (fset #'dashboard-setup-startup-hook #'pg/dashboard-setup-startup-hook)
  (pg/dashboard-setup-startup-hook))

(require 'page-break-lines)

(provide 'pg-ui)
