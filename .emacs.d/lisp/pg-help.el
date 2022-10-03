;;; pg-help.el -*- lexical-binding: t; -*-
;; Author: Philippe Gabriel

(straight-use-package 'helpful)
(unless (fboundp 'helpful-callable)
  (autoload #'helpful-callable "helpful" nil t))
(global-set-key [remap describe-function] #'helpful-callable)
(unless (fboundp 'helpful-command)
  (autoload #'helpful-command "helpful" nil t))
(global-set-key [remap describe-command] #'helpful-command)
(unless (fboundp 'helpful-variable)
  (autoload #'helpful-variable "helpful" nil t))
(global-set-key [remap describe-variable] #'helpful-variable)
(unless (fboundp 'helpful-key)
  (autoload #'helpful-key "helpful" nil t))
(global-set-key [remap describe-key] #'helpful-key)
(unless (fboundp 'helpful-symbol)
  (autoload #'helpful-symbol "helpful" nil t))
(global-set-key [remap describe-symbol] #'helpful-symbol)
(with-eval-after-load 'general
  (pg/leader-keys
    "h" '(:ignore t :which-key "help")
    "hk" '(helpful-key :which-key "key")
    "hf" '(helpful-callable :which-key "command")
    "hv" '(helpful-variable :which-key "variable")
    "hb" '(describe-bindings :which-key "bindings")
    "hc" '(describe-face :which-key "face")
    "hp" '(describe-package :which-key "package")
    "hk" '(helpful-kill-buffers :which-key "quit")))

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
  (variable-pitch-mode 1)
  (visual-line-mode 1))

(require 'info)
(with-eval-after-load 'info
  (add-hook 'Info-mode-hook #'pg/Info-mode-setup))

(defun pg/docs-visual-fill ()
  "Applies text soft wrap."
  (pg/customize-set-variables
   '((visual-fill-column-width . 150)
     (visual-fill-column-center-text . t)))
  (visual-fill-column-mode 1))

(straight-use-package 'visual-fill-column)
(unless (fboundp 'visual-fill-column-mode)
  (autoload #'visual-fill-column-mode "visual-fill-column" nil t))
(dolist (mode '(org-mode-hook
                gfm-view-mode-hook
                Info-mode-hook
                eww-mode-hook))
  (add-hook mode #'pg/docs-visual-fill))

(provide 'pg-help)
