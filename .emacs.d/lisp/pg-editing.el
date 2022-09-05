;;; pg-editing.el -*- lexical-binding: t; -*-
;; Author: Philippe Gabriel

(setq tab-width 4                     ; Set tab length
      custom-buffer-indent 2
      display-line-numbers-type 'relative)
(setq-default indent-tabs-mode nil    ; Disable tab caracter
              fill-column 80)         ; 80 caracter column indicator
(show-paren-mode 1)                   ; Enable delimiters matching
(save-place-mode 1)                   ; Remembers last cursor placement in file
(column-number-mode)                  ; Show column numbers
(mouse-avoidance-mode 'banish)        ; No mouse allowed
(global-display-line-numbers-mode 1)  ; Show line numbers
(add-hook 'prog-mode-hook #'display-fill-column-indicator-mode)
(add-hook 'compilation-filter-hook
          #'(lambda () (ansi-color-apply-on-region (point-min) (point-max))))

(dolist (mode '(org-mode-hook         ; Disable line numbers for some modes
                Info-mode-hook
                eww-mode-hook
                term-mode-hook
                coming-mode-hook
                gfm-view-mode-hook
                compilation-mode-hook
                dashboard-mode-hook
                eshell-mode-hook
                sql-interactive-mode-hook
                pdf-view-mode-hook
                telega-root-mode-hook
                telega-chat-mode
                telega-image-mode
                sokoban-mode-hook
                doc-view-mode-hook
                mu4e-main-mode-hook
                Man-mode-hook
                simple-mpc-mode-hook
                treemacs-mode-hook
                vterm-mode-hook
                geiser-repl-mode-hook
                slack-mode-hook
                shell-mode-hook))
  (add-hook mode #'(lambda() (display-line-numbers-mode 0))))

(set-face-attribute 'default nil :font "JetBrains Mono" :weight 'light :height 120)
(set-face-attribute 'fixed-pitch nil :font "JetBrains Mono" :weight 'light)
(set-face-attribute 'variable-pitch nil :font "Iosevka Aile" :weight 'regular)

(set-face-attribute 'italic nil :slant 'italic)

(use-package ligature
  :straight '(ligature :type git
                       :host github
                       :repo "mickeynp/ligature.el")
  :init
  (require 'ligature)
  :config
  (ligature-set-ligatures 't '("++" "--" "/=" "&&" "||" "||=" "->" "=>" "::" "__" "==" "===" "!=" "=/=" "!=="
                               "<=" ">=" "<=>" "/*" "*/" "//" "///" "\\n" "\\\\" "<<" "<<<" "<<=" ">>" ">>>" ">>="
                               "|=" "^=" "**" "?." "</" "<!--" "</>" "-->" "/>" "www" "##" "###" "####" "#####"
                               "######" "--" "---" "----" "-----" "------" "====" "=====" "======" "[]" "<>" "<~>"
                               "??" ".." "..." "=~" "!~" ":=" "..<" "!!" ":::" "=!=" "=:=" "<:<" "..=" "::<"
                               "#{" "#(" "#_" "#_(" "#?" "#:" ".-" ";;" "~@" "<-" "#{}" "|>" "=>>" "=<<" ">=>" "<=<"
                               "=>=" "=<=" "<$" "<$>" "$>" "<+" "<+>" "+>" "<*" "<*>" "*>" "<|>" ".=" "#=" "+++" "***"
                               ":>:" ":<:" "<|||" "<||" "<|" "||>" "|||>" "[|" "|]" "~-" "~~" "%%" "/\\" "\\/" "-|" "_|"
                               "_|_" "|-" "||-" ":>" ":<" ">:" "<:" "::>" "<::" ">::" "{|" "|}" "#[" "]#" "::="
                               "#!" "#=" "->>" ">-" ">>-" "->-" "->>-" "=>>=" ">>->" ">>=>" "|->" "|=>" "~>" "~~>" "//=>"
                               "<<-" "-<" "-<<" "-||" "-<-" "-<<-" "=<" "=|" "=||" "=<<=" "<-<<" "<=<<" "<-|" "<=|" "<~"
                               "<~~" "<=//" "<->" "<<=>>" "|-|-|" "|=|=|" "/=/"))
  (global-ligature-mode 't))

(use-package rainbow-delimiters
  :straight t
  :init
  (require 'rainbow-delimiters)
  :hook
  (prog-mode . rainbow-delimiters-mode))

(use-package abbrev
  :straight nil
  :init
  (require 'abbrev)
  :after diminish
  :diminish abbrev-mode)

(use-package highlight-indent-guides
  :straight t
  :init
  (require 'highlight-indent-guides)
  :hook
  (prog-mode . highlight-indent-guides-mode)
  :custom
  (highlight-indent-guides-responsive 'stack)
  (highlight-indent-guides-method 'character))

(use-package smartparens
  :straight t
  :init
  (require 'smartparens)
  :after diminish
  :diminish smartparens-mode
  :config
  (smartparens-global-mode))

(use-package outshine
  :straight t
  :init
  (require 'outshine)
  :hook
  (prog-mode . outshine-mode))

(defun pg/selectric-type-sound ()
  "Make the sound of the printing element hitting the paper."
  (progn
    (selectric-make-sound (format "%sselectric-move.wav" selectric-files-path))
    (unless (minibufferp)
      (if (= (current-column) (current-fill-column))
          (selectric-make-sound (format "%sping.wav" selectric-files-path))))))

(use-package selectric-mode
  :disabled
  :straight t
  :init
  (require 'selectric-mode)
  (fset #'selectric-type-sound #'pg/selectric-type-sound))

(use-package rainbow-mode
  :straight t
  :init
  (require 'rainbow-mode)
  :after diminish
  :diminish rainbow-mode
  :hook
  ((org-mode
    emacs-lisp-mode
    web-mode
    typescript-mode
    css-mode
    scss-mode
    less-css-mode) . rainbow-mode))

(use-package emojify
  :straight t
  :init
  (require 'emojify)
  :config
  (global-emojify-mode))

(defun pg/evil-hook()
  "Modes that should start in emacs mode"
  (dolist (mode '(custom-mode
                  eshell-mode
                  git-rebase-mode
                  erc-mode
                  circe-server-mode
                  circe-chat-mode
                  circe-query-mode
                  sauron-mode
                  term-mode))
    (add-to-list 'evil-emacs-state-modes mode)))

(use-package evil
  :straight t
  :preface
  (customize-set-variable 'evil-want-keybinding nil)
  :init
  (require 'evil)
  :hook
  (evil-mode . pg/evil-hook)
  :custom
  (evil-want-integration t)
  (evil-want-C-u-scroll t)
  (evil-want-C-i-jump nil)
  ;; (evil-want-Y-yank-to-eol t)
  (evil-want-fine-undo t)
  :bind
  (:map evil-insert-state-map
        ("C-g" . evil-normal-state))
  :config
  (customize-set-variable 'evil-want-Y-yank-to-eol t)
  (evil-mode 1)
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)
  (dolist (mode '(messages-buffer-mode
                  dashboard-mode))
    (evil-set-initial-state mode 'normal)))

(use-package evil-collection
  :straight t
  :init
  (require 'evil-collection)
  :after (evil diminish)
  :diminish evil-collection-unimpaired-mode
  :config
  (evil-collection-init))

(provide 'pg-editing)
