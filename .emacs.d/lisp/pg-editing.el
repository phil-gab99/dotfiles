(require 'pg-startup)

(setq tab-width 4)                    ; Set tab length
(setq custom-buffer-indent 2)
(setq-default indent-tabs-mode nil)   ; Disable tab caracter
(show-paren-mode 1)                   ; Enable delimiters matching
(save-place-mode 1)                   ; Remembers last cursor placement in file
(column-number-mode)                  ; Show column numbers
(mouse-avoidance-mode 'banish)        ; No mouse allowed
(global-display-line-numbers-mode 1)  ; Show line numbers
(setq display-line-numbers-type 'relative)
(setq-default fill-column 80)         ; 80 caracter column indicator
(add-hook 'prog-mode-hook 'display-fill-column-indicator-mode)
(add-hook 'compilation-filter-hook
          (lambda () (ansi-color-apply-on-region (point-min) (point-max))))

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
  (add-hook mode (lambda() (display-line-numbers-mode 0))))

(set-face-attribute 'default nil :font "JetBrains Mono" :weight 'light :height 120)
(set-face-attribute 'fixed-pitch nil :font "JetBrains Mono" :weight 'light)
(set-face-attribute 'variable-pitch nil :font "Iosevka Aile" :weight 'regular)

(set-face-attribute 'italic nil :slant 'italic)

(use-package ligature
  :straight '(ligature :type git
                       :host github
                       :repo "mickeynp/ligature.el")
  :config
  ;; Enable ligatures
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
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package abbrev
  :straight nil
  :diminish abbrev-mode)

(use-package highlight-indent-guides
  :straight t
  :hook (prog-mode . highlight-indent-guides-mode)
  :custom 
  (highlight-indent-guides-responsive 'stack)
  (highlight-indent-guides-method 'character))

(use-package undo-fu
  :straight t)

(use-package smartparens
  :straight t
  :diminish smartparens-mode
  :config
  (smartparens-global-mode))

(with-eval-after-load 'smartparens
  (sp-with-modes
      '(prog-mode)
    (sp-local-pair "{" nil :post-handlers '(:add ("||\n[i]" "RET")))))

(use-package outshine
  :straight t
  :hook (prog-mode . outshine-mode))

(defun pg/selectric-type-sound ()
  "Make the sound of the printing element hitting the paper."
  (progn
    (selectric-make-sound (format "%sselectric-move.wav" selectric-files-path))
    (unless (minibufferp)
      (if (= (current-column) (current-fill-column))
          (selectric-make-sound (format "%sping.wav" selectric-files-path))))))

(use-package selectric-mode
  :straight t
  :config
  (fset #'selectric-type-sound #'pg/selectric-type-sound))

(use-package rainbow-mode
  :straight t
  :diminish rainbow-mode
  :hook ((org-mode
          emacs-lisp-mode
          web-mode
          typescript-mode
          css-mode
          scss-mode
          less-css-mode) . rainbow-mode))

(use-package emojify
  :straight t
  :config
  (global-emojify-mode))

;; Function for modes that should start in emacs mode
(defun pg/evil-hook()
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
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  (setq evil-want-Y-yank-to-eol t)
  (setq evil-want-fine-undo t)
  (evil-mode 1)
  :hook (evil-mode . pg/evil-hook)
  :custom
  (evil-undo-system 'undo-fu)
  :config
  (evil-set-register ?j [?f ?  ?s return escape]) ;; break at point

  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)

  ;; Visual line motions outside visual-line mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)
  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

(use-package evil-collection
  :straight t
  :after evil
  :diminish evil-collection-unimpaired-mode
  :config
  (evil-collection-init))

(provide 'pg-editing)
