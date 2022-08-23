(use-package eshell-git-prompt
  :straight t
  :after eshell)

(use-package eshell-syntax-highlighting
  :straight t
  :after eshell
  :config
  (eshell-syntax-highlighting-global-mode 1))

(defun pg/esh-autosuggest-setup ()
  (require 'company)
  (set-face-foreground 'company-preview-common nil)
  (set-face-background 'company-preview nil))

(use-package esh-autosuggest
  :straight t
  :hook (eshell-mode . esh-autosuggest-mode)
  :custom
  (esh-autosuggest-delay 0.5)
  :config
  (require 'esh-autosuggest)
  (pg/esh-autosuggest-setup))

(defun pg/configure-eshell ()
  ;; Save command history when commands are entered
  (add-hook 'eshell-pre-command-hook 'eshell-save-some-history)

  ;; Truncate buffer for performance
  (add-to-list 'eshell-output-filter-functions 'eshell-truncate-buffer)

  ;; Bind some useful keys for evil-mode
  (evil-define-key '(normal insert visual) eshell-mode-map (kbd "<home>") 'eshell-bol)
  (evil-normalize-keymaps)

  (local-unset-key (kbd "M-<tab>"))
  (corfu-mode)

  (setq eshell-history-size 10000
        eshell-buffer-maximum-lines 10000
        eshell-hist-ignoredups t
        eshell-scroll-to-bottom-on-input t))

(require 'em-tramp)
(use-package eshell
  :straight nil
  :hook (eshell-first-time-mode . pg/configure-eshell)
  :custom
  (eshell-prefer-lisp-functions t)
  :config
  (eshell-git-prompt-use-theme 'multiline2))

(use-package vterm
  :straight nil)

(provide 'pg-shell)
