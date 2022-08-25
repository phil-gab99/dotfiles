(require 'savehist)
(with-eval-after-load 'savehist
  (savehist-mode))

(require 'marginalia)
(with-eval-after-load 'vertico
  (customize-set-variable 'marginalia-annotators '(marginalia-annotators-heavy
                                                   marginalia-annotators-light
                                                   nil))
  (marginalia-mode))

(unless (fboundp 'consult-line)
  (autoload #'consult-line "consult" nil t))
(unless (fboundp 'consult-buffer)
  (autoload #'consult-buffer "consult" nil t))
(unless (fboundp 'consult-history)
  (autoload #'consult-history "consult" nil t))
;; (require 'consult)
(with-eval-after-load 'consult
  (bind-keys :package consult
             ("C-s" . consult-line)
             ("C-x b" . consult-buffer)
             :map minibuffer-local-map
             ("C-r" . consult-history)))

(require 'orderless)
(with-eval-after-load 'vertico
  (customize-set-variable 'completion-styles '(orderless))
  (customize-set-variable 'completion-category-defaults nil)
  (customize-set-variable 'orderless-skip-highlighting nil)
  (customize-set-variable 'completion-category-overrides '((file (styles basic partial-completion)))))

(unless (fboundp 'corfu-next)
  (autoload #'corfu-next "corfu" nil t))
(unless (fboundp 'corfu-previous)
  (autoload #'corfu-previous "corfu" nil t))
;; (require 'corfu)
(with-eval-after-load 'corfu
  (bind-keys :package corfu :map corfu-map
             ("C-j" . corfu-next)
             ("C-k" . corfu-previous))
(customize-set-variable 'corfu-cycle t))

(unless (fboundp 'vertico-next)
  (autoload #'vertico-next "vertico" nil t))
(unless (fboundp 'vertico-previous)
  (autoload #'vertico-previous "vertico" nil t))
;; (require 'vertico)
(with-eval-after-load 'vertico
  (bind-keys :package vertico :map vertico-map
             ("C-j" . vertico-next)
             ("C-k" . vertico-previous))
  (customize-set-variable 'vertico-cycle t)
  (vertico-mode))

(unless (fboundp 'embark-act)
  (autoload #'embark-act "embark" nil t))
;; (require 'embark)
(with-eval-after-load 'embark
  (bind-keys :package embark
             ("C-S-a" . embark-act)
             :map minibuffer-local-map
             ("C-d" . embark-act))
  (customize-set-variable 'embark-confirm-act-all nil)
  (setq embark-action-indicator
        (lambda (map)
          (which-key--show-keymap "Embark" map nil nil 'no-paging)
          #'which-key--hide-popup-ignore-command)
        embark-become-indicator embark-action-indicator))

(straight-use-package '(embark-consult :host github
                                       :repo "oantolin/embark"
                                       :files ("embark-consult.el")))
(require 'embark-consult)
(with-eval-after-load 'embark
  (with-eval-after-load 'consult
    (add-hook 'embark-collect-mode-hook #'consult-preview-at-point-mode)))

(require 'prescient)

(require 'which-key)
(with-eval-after-load 'which-key
  (which-key-mode)
  (customize-set-variable 'which-key-idle-delay 1)
  (diminish 'which-key-mode))

(unless (fboundp 'helm-previous-line)
  (autoload #'helm-previous-line "helm" nil t))
(unless (fboundp 'helm-next-line)
  (autoload #'helm-next-line "helm" nil t))
;; (require 'helm)
(with-eval-after-load 'lsp-java
  (bind-keys :package helm :map helm-map
             ("C-k" . helm-previous-line)
             ("C-j" . helm-next-line))
  (if (and (eq #'java-mode major-mode) (memq #'lsp-mode local-minor-modes))
      (helm-mode 1)
    (helm-mode 0)))

(provide 'pg-completion)
