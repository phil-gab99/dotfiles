(straight-use-package 'helpful)
(dolist (call '(helpful-callable
                helpful-command
                helpful-variable
                helpful-key))
  (unless (fboundp call)
    (autoload call "helpful" nil t)))
(with-eval-after-load 'helpful
  (global-set-key [remap describe-function] #'helpful-callable)
  (global-set-key [remap describe-command] #'helpful-command)
  (global-set-key [remap describe-variable] #'helpful-variable)
  (global-set-key [remap describe-key] #'helpful-key))

(defun pg/Info-mode-setup ()
  "Defining some behaviours for the major info-mode."
  (auto-fill-mode 0)
  (setq-local face-remapping-alist '((default (:height 1.5) default)
                                     (fixed-pitch (:height 1.5) fixed-pitch)
                                     (info-menu-header (:height 1.5) info-menu-header)
                                     (info-title-1 (:height 1.05) info-title-1)
                                     (info-title-2 (:height 1.15) info-title-2)
                                     (info-title-3 (:height 1.15) info-title-3)
                                     (info-title-4 (:height 2.0) info-title-4)))
  (set-face-attribute 'Info-quoted nil :foreground "orange" :inherit 'fixed-pitch)
  (variable-pitch-mode 1)
  (visual-line-mode 1))

(require 'info)
(with-eval-after-load 'info
  (add-hook 'Info-mode-hook #'pg/Info-mode-setup))

(defun pg/docs-visual-fill ()
  "Applies text soft wrap."
  (setq visual-fill-column-width 150
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(straight-use-package 'visual-fill-column)
(require 'visual-fill-column)
(with-eval-after-load 'visual-fill-column
  (dolist (mode '(org-mode-hook
                  gfm-view-mode-hook
                  Info-mode-hook
                  eww-mode-hook))
    (add-hook mode #'pg/docs-visual-fill)))

(provide 'pg-help)
