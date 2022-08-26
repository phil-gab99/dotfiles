(straight-use-package 'eshell-git-prompt)
(with-eval-after-load 'eshell
  (require 'eshell-git-prompt)
  (with-eval-after-load 'eshell-git-prompt
    (eshell-git-prompt-use-theme 'multiline2)))

(straight-use-package 'eshell-syntax-highlighting)
(with-eval-after-load 'eshell
  (require 'eshell-syntax-highlighting)
  (with-eval-after-load 'eshell-syntax-highlighting
    (eshell-syntax-highlighting-global-mode t)))

(defun pg/esh-autosuggest-setup ()
  "Eshell autosuggest setup."
  (require 'company)
  (set-face-foreground 'company-preview-common nil)
  (set-face-background 'company-preview nil))

(straight-use-package 'esh-autosuggest)
(with-eval-after-load 'eshell
  (require 'esh-autosuggest)
  (with-eval-after-load 'esh-autosuggest
    (add-hook 'eshell-mode-hook #'esh-autosuggest-mode)
    (customize-set-variable 'esh-autosuggest-delay 0.5)
    (pg/esh-autosuggest-setup)))

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

(require 'eshell)
(with-eval-after-load 'eshell
  (require 'em-tramp)
  (add-hook 'eshell-first-time-mode-hook #'pg/configure-eshell)
  (customize-set-variable 'eshell-prefer-lisp-functions t))

(require 'vterm)

(provide 'pg-shell)
