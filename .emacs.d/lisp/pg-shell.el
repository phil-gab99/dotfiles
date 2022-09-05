;;; pg-shell.el -*- lexical-binding: t; -*-
;; Author: Philippe Gabriel

(use-package eshell-git-prompt
  :straight t
  :init
  (require 'eshell-git-prompt)
  :after eshell
  :config
  (eshell-git-prompt-use-theme 'multiline2))

(use-package eshell-syntax-highlighting
  :straight t
  :init
  (require 'eshell-syntax-highlighting)
  :after eshell
  :custom
  (eshell-syntax-highlighting-global-mode t))

(defun pg/esh-autosuggest-setup ()
  "Eshell autosuggest setup."
  (require 'company)
  (set-face-foreground 'company-preview-common nil)
  (set-face-background 'company-preview nil))

(use-package esh-autosuggest
  :straight t
  :init
  (require 'esh-autosuggest)
  :after eshell
  :hook
  (eshell-mode . esh-autosuggest-mode)
  :custom
  (esh-autosuggest-delay 0.5)
  :bind
  (:map esh-autosuggest-active-map
        ("<tab>" . company-complete-selection))
  :config
  (pg/esh-autosuggest-setup))

(defun pg/configure-eshell ()
  "Eshell setup."
  ;; Save command history when commands are entered
  (add-hook 'eshell-pre-command-hook #'eshell-save-some-history)

  ;; Truncate buffer for performance
  (add-to-list 'eshell-output-filter-functions 'eshell-truncate-buffer)

  ;; Bind some useful keys for evil-mode
  (evil-define-key '(normal insert visual) eshell-mode-map (kbd "<home>") 'eshell-bol)
  (evil-normalize-keymaps)

  (corfu-mode)

  (setq eshell-history-size 10000
        eshell-buffer-maximum-lines 10000
        eshell-hist-ignoredups t
        eshell-scroll-to-bottom-on-input t))

(use-package eshell
  :straight nil
  :init
  (require 'eshell)
  (require 'em-tramp)
  :hook
  (eshell-first-time-mode . pg/configure-eshell)
  :custom
  (eshell-prefer-lisp-functions t))

(use-package vterm
  :straight nil
  :init
  (require 'vterm))

(provide 'pg-shell)
