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

(straight-use-package '(ligature :type git
                                 :host github
                                 :repo "mickeynp/ligature.el"))
(require 'ligature)
(with-eval-after-load 'ligature
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

(require 'rainbow-delimiters)
(with-eval-after-load 'rainbow-delimiters
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(require 'abbrev)
(with-eval-after-load 'abbrev
  (diminish 'abbrev-mode))

(straight-use-package 'highlight-indent-guides)
(require 'highlight-indent-guides)
(with-eval-after-load 'highlight-indent-guides
  (add-hook 'prog-mode-hook #'highlight-indent-guides-mode)
  (customize-set-variable 'highlight-indent-guides-responsive 'stack)
  (customize-set-variable 'highlight-indent-guides-method 'character))

(require 'smartparens)
(with-eval-after-load 'smartparens
  (diminish 'smartparens-mode)
  (smartparens-global-mode)
  (sp-with-modes
      '(prog-mode)
    (sp-local-pair "{" nil :post-handlers '(:add ("||\n[i]" "RET")))))

(require 'outshine)
(with-eval-after-load 'outshine
  (add-hook 'prog-mode-hook #'outshine-mode))

(defun pg/selectric-type-sound ()
  "Make the sound of the printing element hitting the paper."
  (progn
    (selectric-make-sound (format "%sselectric-move.wav" selectric-files-path))
    (unless (minibufferp)
      (if (= (current-column) (current-fill-column))
          (selectric-make-sound (format "%sping.wav" selectric-files-path))))))

(straight-use-package 'selectric-mode)
(require 'selectric-mode)
(with-eval-after-load 'selectric-mode
   (fset #'selectric-type-sound #'pg/selectric-type-sound))

(require 'rainbow-mode)
(with-eval-after-load 'rainbow-mode
  (diminish 'rainbow-mode)
  (dolist (mode '(org-mode-hook
                  emacs-lisp-mode-hook
                  web-mode-hook
                  typescript-mode-hook
                  css-mode-hook
                  scss-mode-hook
                  less-css-mode-hook))
    (add-hook mode #'rainbow-mode)))

(require 'emojify)
(with-eval-after-load 'emojify
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

(customize-set-variable 'evil-want-keybinding nil)
(require 'evil)
(with-eval-after-load 'evil
  (customize-set-variable 'evil-want-integration t)
  (customize-set-variable 'evil-want-C-u-scroll t)
  (customize-set-variable 'evil-want-C-i-jump nil)
  (customize-set-variable 'evil-want-Y-yank-to-eol t)
  (customize-set-variable 'evil-want-fine-undo t)
  (evil-mode 1)
  (add-hook 'evil-mode-hook #'pg/evil-hook)
  (bind-key "C-g" #'evil-normal-state evil-insert-state-map)
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)
  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

(require 'evil)
(require 'evil-collection)
(with-eval-after-load 'evil-collection
  (diminish 'evil-collection-unimpaired-mode)
  (evil-collection-init))

(provide 'pg-editing)
