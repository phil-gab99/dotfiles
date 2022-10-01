;;; pg-shell.el -*- lexical-binding: t; -*-
;; Author: Philippe Gabriel

(straight-use-package 'eshell-git-prompt)
(with-eval-after-load 'eshell
  (require 'eshell-git-prompt))
(with-eval-after-load 'eshell-git-prompt
  (eshell-git-prompt-use-theme 'multiline2))

(straight-use-package 'eshell-syntax-highlighting)
(with-eval-after-load 'eshell
  (require 'eshell-syntax-highlighting))
(with-eval-after-load 'eshell-syntax-highlighting
  (customize-set-variable 'eshell-syntax-highlighting-global-mode t))

(straight-use-package 'esh-autosuggest)
(with-eval-after-load 'eshell
  (unless (fboundp 'esh-autosuggest-mode)
    (autoload #'esh-autosuggest-mode "esh-autosuggest" nil t))
  (add-hook 'eshell-mode-hook #'esh-autosuggest-mode))
(with-eval-after-load 'esh-autosuggest
  (customize-set-variable 'esh-autosuggest-delay 0.5))

(defun pg/configure-eshell ()
  "Eshell setup."
  (with-eval-after-load 'evil
    (evil-define-key '(normal insert visual) eshell-mode-map
      (kbd "<home>") #'eshell-bol)
    (evil-normalize-keymaps))

  (with-eval-after-load 'corfu
    (corfu-mode))

  (require 'em-hist)
  (with-eval-after-load 'em-hist
    (pg/customize-set-variables
     '((eshell-history-size . 10000)
       (eshell-hist-ignoredups . t)))
    (require 'esh-cmd)
    (with-eval-after-load 'esh-cmd
      (add-hook 'eshell-pre-command-hook #'eshell-save-some-history)))

  (require 'esh-mode)
  (with-eval-after-load 'esh-mode
    (add-to-list 'eshell-output-filter-functions 'eshell-truncate-buffer)
    (pg/customize-set-variables
     '((eshell-buffer-maximum-lines . 10000)
       (eshell-scroll-to-bottom-on-input . t)))))

(require 'esh-mode)
(with-eval-after-load 'esh-mode
  (add-hook 'eshell-first-time-mode-hook #'pg/configure-eshell))
(unless (fboundp 'eshell)
  (autoload #'eshell "eshell" nil t))
(with-eval-after-load 'eshell
  (require 'em-tramp)
  (customize-set-variable 'eshell-prefer-lisp-functions t))
(with-eval-after-load 'general
  (pg/leader-keys
    "pe" '(eshell :which-key "eshell")))

(unless pg/is-guix-system
  (straight-use-package 'vterm))
(unless (fboundp 'vterm)
  (autoload #'vterm "vterm" nil t))
(with-eval-after-load 'general
  (pg/leader-keys
    "pv" '(vterm :which-key "vterm")))

(provide 'pg-shell)
