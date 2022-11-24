;;; pg-completion.el -*- lexical-binding: t; -*-
;; Author: Philippe Gabriel

(require 'savehist)
(with-eval-after-load 'savehist
  (savehist-mode))

(straight-use-package 'marginalia)
(with-eval-after-load 'vertico
  (require 'marginalia))
(with-eval-after-load 'marginalia
  (customize-set-variable 'marginalia-annotators '(marginalia-annotators-heavy
                                                   marginalia-annotators-light
                                                   nil))
  (marginalia-mode))

(straight-use-package 'consult)
(unless (fboundp 'consult-line)
  (autoload #'consult-line "consult" nil t))
(global-set-key (kbd "C-s") #'consult-line)
(unless (fboundp 'consult-buffer)
  (autoload #'consult-buffer "consult" nil t))
(global-set-key (kbd "C-x b") #'consult-buffer)
(with-eval-after-load 'consult
  (customize-set-variable 'consult-buffer-sources '(consult--source-buffer))
  (consult-customize consult-buffer
                     consult-theme
                     :preview-key nil)
  (define-key minibuffer-local-map (kbd "C-r") #'consult-history))

(straight-use-package 'orderless)
(with-eval-after-load 'vertico
  (require 'orderless))
(with-eval-after-load 'orderless
  (pg/customize-set-variables
   '((completion-styles . (orderless))
     (completion-category-defaults . nil)
     (orderless-skip-highlighting . nil)
     (completion-category-overrides . ((file (styles basic partial-completion)))))))

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
  (customize-set-variable 'corfu-cycle t))

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
  (customize-set-variable 'vertico-cycle t)
  (vertico-mode))

(straight-use-package 'embark)
(with-eval-after-load 'vertico
  (unless (fboundp 'embark-act)
    (autoload #'embark-act "embark" nil t))
  (global-set-key (kbd "C-S-a") #'embark-act)
  (define-key minibuffer-local-map (kbd "C-d") #'embark-act))
(with-eval-after-load 'embark
  (customize-set-variable 'embark-confirm-act-all nil)
  (setq embark-action-indicator
        (lambda
          (map)
          (which-key--show-keymap "Embark" map nil nil 'no-paging)
          #'which-key--hide-popup-ignore-command)
        embark-become-indicator embark-action-indicator))

(straight-use-package '(embark-consult :host github
                                       :repo "oantolin/embark"
                                       :files ("embark-consult.el")))
(add-hook 'embark-collect-mode-hook #'consult-preview-at-point-mode)
(with-eval-after-load 'embark
  (with-eval-after-load 'consult
    (require 'embark-consult)))

(straight-use-package 'prescient)

(straight-use-package 'which-key)
(require 'which-key)
(with-eval-after-load 'which-key
  (customize-set-variable 'which-key-idle-delay 1)
  (which-key-mode)
  (if (fboundp 'diminish)
      (diminish #'which-key-mode)
    (with-eval-after-load 'diminish
      (diminish #'which-key-mode))))

(straight-use-package 'helm)
(unless (fboundp 'helm-next-line)
  (autoload #'helm-next-line "helm" nil t))
(unless (fboundp 'helm-previous-line)
  (autoload #'helm-previous-line "helm" nil t))
(if (boundp 'helm-map)
    (progn
      (define-key helm-map (kbd "C-j") #'helm-next-line)
      (define-key helm-map (kbd "C-k") #'helm-previous-line))
  (with-eval-after-load 'helm
    (define-key helm-map (kbd "C-j") #'helm-next-line)
    (define-key helm-map (kbd "C-k") #'helm-previous-line)))

(provide 'pg-completion)
