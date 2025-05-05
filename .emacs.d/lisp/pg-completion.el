;;; pg-completion.el -*- lexical-binding: t; -*-
;; Author: Philippe Gabriel

(require 'savehist)
(with-eval-after-load 'savehist
  (savehist-mode))

(straight-use-package 'marginalia)
(with-eval-after-load 'vertico
  (require 'marginalia))
(with-eval-after-load 'marginalia
  (setopt marginalia-annotators '(marginalia-annotators-heavy
                                  marginalia-annotators-light
                                  nil))
  (marginalia-mode))

(straight-use-package 'consult)
(unless (fboundp 'consult-line)
  (autoload #'consult-line "consult" nil t))
(global-set-key (kbd "C-s") #'consult-line)
(unless (fboundp 'consult-ripgrep)
  (autoload #'consult-ripgrep "consult" nil t))
(global-set-key (kbd "C-M-s") #'consult-ripgrep)
(unless (fboundp 'consult-buffer)
  (autoload #'consult-buffer "consult" nil t))
(global-set-key (kbd "C-x b") #'consult-buffer)
(with-eval-after-load 'consult
  (setopt consult-buffer-sources '(consult--source-buffer))
  (consult-customize consult-buffer
                     consult-theme
                     :preview-key nil)
  (define-key minibuffer-local-map (kbd "C-r") #'consult-history))

(straight-use-package 'orderless)
(with-eval-after-load 'vertico
  (require 'orderless))
(with-eval-after-load 'orderless
  (setopt completion-styles '(orderless)
          completion-category-defaults nil
          orderless-skip-highlighting nil
          completion-category-overrides '((file (styles basic partial-completion)))))

(straight-use-package 'corfu)
(unless (fboundp 'corfu-next)
  (autoload #'corfu-next "corfu" nil t))
(unless (fboundp 'corfu-previous)
  (autoload #'corfu-previous "corfu" nil t))
(if (boundp 'corfu-map)
    (progn
      (define-key corfu-map (kbd "C-j") #'corfu-next)
      (define-key corfu-map (kbd "C-k") #'corfu-previous))
  (with-eval-after-load 'corfu
    (define-key corfu-map (kbd "C-j") #'corfu-next)
    (define-key corfu-map (kbd "C-k") #'corfu-previous)))
(unless (fboundp 'corfu-mode)
  (autoload #'corfu-mode "corfu" nil t))
(with-eval-after-load 'corfu
  (setopt corfu-cycle t))

(straight-use-package 'vertico)
(require 'vertico)
(unless (fboundp 'vertico-next)
  (autoload #'vertico-next "vertico" nil t))
(unless (fboundp 'vertico-previous)
  (autoload #'vertico-previous "vertico" nil t))
(if (boundp 'vertico-map)
    (progn
      (define-key vertico-map (kbd "C-j") #'vertico-next)
      (define-key vertico-map (kbd "C-k") #'vertico-previous))
  (with-eval-after-load 'vertico
    (define-key vertico-map (kbd "C-j") #'vertico-next)
    (define-key vertico-map (kbd "C-k") #'vertico-previous)))
(with-eval-after-load 'vertico
  (setopt vertico-cycle t)
  (vertico-mode))

(straight-use-package 'embark)
(with-eval-after-load 'vertico
  (unless (fboundp 'embark-act)
    (autoload #'embark-act "embark" nil t))
  (global-set-key (kbd "C-S-a") #'embark-act)
  (define-key minibuffer-local-map (kbd "C-d") #'embark-act))
(with-eval-after-load 'embark
  (setopt embark-confirm-act-all nil)
  (setq embark-action-indicator
        (lambda
          (map)
          (which-key--show-keymap "Embark" map nil nil 'no-paging)
          #'which-key--hide-popup-ignore-command)
        embark-become-indicator embark-action-indicator))

(straight-use-package 'which-key)
(require 'which-key)
(with-eval-after-load 'which-key
  (setopt which-key-idle-delay 1.0)
  (which-key-mode)
  (unless (fboundp 'diminish)
    (autoload #'diminish "diminish" nil t))
  (diminish #'which-key-mode))

(straight-use-package 'helm)
(with-eval-after-load 'helm
  (define-key helm-map (kbd "C-j") #'helm-next-line)
  (define-key helm-map (kbd "C-k") #'helm-previous-line))

(provide 'pg-completion)
