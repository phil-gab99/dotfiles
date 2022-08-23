(require 'pg-startup)

(use-package helpful
  :straight t
  :commands (helpful-callable helpful-variable helpful-command helpful-key)
  :bind
  ([remap describe-function] . helpful-callable)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-key] . helpful-key))

;; Function for defining some behaviours for the major info-mode
(defun pg/Info-mode-setup ()
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

(use-package info
  :straight nil
  :hook (Info-mode . pg/Info-mode-setup))

;; Turns soft wrap on
(defun pg/org-mode-visual-fill ()
  (setq visual-fill-column-width 150
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :straight t
  :hook ((org-mode gfm-view-mode Info-mode eww-mode) . pg/org-mode-visual-fill))

(provide 'pg-help)
