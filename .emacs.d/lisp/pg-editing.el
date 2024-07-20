;;; pg-editing.el -*- lexical-binding: t; -*-
;; Author: Philippe Gabriel

(setq tab-width 4                     ;; Set tab length
      custom-buffer-indent 2
      display-line-numbers-type 'relative)
(setq-default indent-tabs-mode nil    ;; Disable tab caracter
              fill-column 80)         ;; 80 caracter column indicator
(show-paren-mode 1)                   ;; Enable delimiters matching
(save-place-mode 1)                   ;; Remembers last cursor placement in file
(column-number-mode)                  ;; Show column numbers
(mouse-avoidance-mode 'banish)        ;; No mouse allowed
(global-display-line-numbers-mode 1)  ;; Show line numbers
(add-hook 'prog-mode-hook #'display-fill-column-indicator-mode)
(require 'ansi-color)
(add-hook 'compilation-filter-hook 'ansi-color-compilation-filter)

(dolist (mode '(compilation-mode-hook
                Man-mode-hook
                comint-mode-hook
                etc-authors-mode-hook))
  (add-hook mode #'(lambda ()
                     (display-line-numbers-mode 0))))

(set-face-attribute 'default nil :font (plist-get pg/user :font-fixed) :weight 'light :height 120)
(set-face-attribute 'fixed-pitch nil :family (plist-get pg/user :font-fixed) :weight 'light)
(set-face-attribute 'variable-pitch nil :family (plist-get pg/user :font-variable) :weight 'regular)

(straight-use-package 'ligature)
(require 'ligature)
(with-eval-after-load 'ligature
  (ligature-set-ligatures 't
                          '("++" "--" "/=" "&&" "||" "||=" "->" "=>" "::" "__"
                            "==" "===" "!=" "=/=" "!==" "<=" ">=" "<=>" "/*"
                            "*/" "//" "///" "\\n" "\\\\" "<<" "<<<" "<<=" ">>"
                            ">>>" ">>=" "|=" "^=" "**" "?." "</" "<!--" "</>"
                            "-->" "/>" "www" "##" "###" "####" "#####" "######"
                            "--" "---" "----" "-----" "------" "====" "====="
                            "======" "[]" "<>" "<~>" "??" ".." "..." "=~" "!~"
                            ":=" "..<" "!!" ":::" "=!=" "=:=" "<:<" "..=" "::<"
                            "#{" "#(" "#_" "#_(" "#?" "#:" ".-" ";;" "~@" "<-"
                            "#{}" "|>" "=>>" "=<<" ">=>" "<=<" "=>=" "=<=" "<$"
                            "<$>" "$>" "<+" "<+>" "+>" "<*" "<*>" "*>" "<|>"
                            ".=" "#=" "+++" "***" ":>:" ":<:" "<|||" "<||" "<|"
                            "||>" "|||>" "[|" "|]" "~-" "~~" "%%" "/\\" "\\/"
                            "-|" "_|" "_|_" "|-" "||-" ":>" ":<" ">:" "<:" "::>"
                            "<::" ">::" "{|" "|}" "#[" "]#" "::=" "#!" "#="
                            "->>" ">-" ">>-" "->-" "->>-" "=>>=" ">>->" ">>=>"
                            "|->" "|=>" "~>" "~~>" "//=>" "<<-" "-<" "-<<" "-||"
                            "-<-" "-<<-" "=<" "=|" "=||" "=<<=" "<-<<" "<=<<"
                            "<-|" "<=|" "<~" "<~~" "<=//" "<->" "<<=>>" "|-|-|"
                            "|=|=|" "/=/"))
  (global-ligature-mode))

(straight-use-package 'rainbow-delimiters)
(unless (fboundp 'rainbow-delimiters-mode)
  (autoload #'rainbow-delimiters-mode "rainbow-delimiters" nil t))
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

(require 'abbrev)
(with-eval-after-load 'abbrev
  (unless (fboundp 'diminish)
    (autoload #'diminish "diminish" nil t))
  (diminish #'abbrev-mode))

(straight-use-package 'highlight-indent-guides)
(unless (fboundp 'highlight-indent-guides-mode)
  (autoload #'highlight-indent-guides-mode "rainbow-delimiters" nil t))
(add-hook 'prog-mode-hook #'highlight-indent-guides-mode)
(with-eval-after-load 'highlight-indent-guides
  (setopt highlight-indent-guides-responsive 'stack
          highlight-indent-guides-method 'character))

(defvar pg/sp-post-command-count 0
  "Number of commands called after a pair has been opened.")

(defun pg/sp-create-newline-and-enter-sexp ()
  "Open a new brace or bracket expression, with relevant newlines and indent. "
  (newline)
  (indent-according-to-mode)
  (previous-line)
  (indent-according-to-mode))

(defun pg/sp-release-newline-post-command ()
  "Remove the hook and reset the post-command count."
  (remove-hook 'post-command-hook 'pg/sp-await-newline-post-command)
  (setq pg/sp-post-command-count 0))

(defun pg/sp-await-newline-post-command ()
  "If command is newline, indent and enter sexp."
  (if (> pg/sp-post-command-count 1)
      (pg/sp-release-newline-post-command)
    (progn
      (setq pg/sp-post-command-count (1+ pg/sp-post-command-count))
      (when (memq this-command
                  '(newline newline-and-indent reindent-then-newline-and-indent))
        (pg/sp-release-newline-post-command)
        (pg/sp-create-newline-and-enter-sexp)))))

(defun pg/sp-await-newline (id action context)
  (when (eq action 'insert)
    (add-hook 'post-command-hook 'pg/sp-await-newline-post-command)))

(straight-use-package 'smartparens)
(require 'smartparens-config)

(sp-pair "(" nil :post-handlers '(:add pg/sp-await-newline))
(sp-pair "{" nil :post-handlers '(:add pg/sp-await-newline))
(sp-pair "[" nil :post-handlers '(:add pg/sp-await-newline))

;; (sp-local-pair 'nxml-mode "<" nil :actions nil)

(smartparens-global-mode)

(unless (fboundp 'diminish)
  (autoload #'diminish "diminish" nil t))
(diminish #'smartparens-mode)

(straight-use-package 'outshine)
(unless (fboundp 'outshine-mode)
  (autoload #'outshine-mode "outshine" nil t))
(add-hook 'prog-mode-hook #'outshine-mode)

(defun pg/selectric-type-sound ()
  "Make the sound of the printing element hitting the paper."
  (progn
    (selectric-make-sound (format "%sselectric-move.wav" selectric-files-path))
    (unless (minibufferp)
      (if (= (current-column) (current-fill-column))
          (selectric-make-sound (format "%sping.wav" selectric-files-path))))))

(straight-use-package 'selectric-mode)
(fset #'selectric-type-sound #'pg/selectric-type-sound)
(unless (fboundp 'selectric-mode)
  (autoload #'selectric-mode "selectric-mode" nil t))

(straight-use-package 'rainbow-mode)
(unless (fboundp 'rainbow-mode)
  (autoload #'rainbow-mode "rainbow-mode" nil t))
(dolist (mode '(org-mode-hook
                emacs-lorg-mode-hook
                org-mode-hook
                typescrorg-mode-hook
                org-mode-hook
                scss-mode-hook
                less-css-mode-hook))
  (add-hook mode #'rainbow-mode))
(with-eval-after-load 'rainbow-mode
  (unless (fboundp 'diminish)
    (autoload #'diminish "diminish" nil t))
  (diminish #'rainbow-mode))

(straight-use-package 'emojify)
(require 'emojify)
(with-eval-after-load 'emojify
  (global-emojify-mode))

(defun pg/evil-hook()
  "Configuration of some default modes."
  (dolist (mode '(messages-buffer-mode
                  dashboard-mode))
    (evil-set-initial-state mode 'normal))
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

(straight-use-package 'evil)
(setopt evil-want-keybinding nil)
(add-hook 'evil-mode-hook #'pg/evil-hook)
(require 'evil)
(with-eval-after-load 'evil
  (setopt evil-want-integration t
          evil-want-C-u-scroll t
          evil-want-C-i-jump nil
          evil-want-Y-yank-to-eol t
          evil-want-fine-undo t
          evil-undo-system #'undo-redo)
  (unless (fboundp 'evil-normal-state)
    (autoload #'evil-normal-state "evil-states"))
  (define-key evil-insert-state-map (kbd "C-g") #'evil-normal-state)
  (evil-mode 1)
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line))

(straight-use-package 'evil-collection)
(with-eval-after-load 'evil
  (require 'evil-collection))
(with-eval-after-load 'evil-collection
  (evil-collection-init)
  (unless (fboundp 'diminish)
    (autoload #'diminish "diminish" nil t))
  (diminish #'evil-collection-unimpaired-mode))

(straight-use-package 'editorconfig)
(unless (fboundp 'editorconfig-mode)
  (autoload #'editorconfig-mode "editorconfig" nil t))
(editorconfig-mode 1)
(with-eval-after-load 'editorconfig
  (unless (fboundp 'diminish)
    (autoload #'diminish "diminish" nil t))
  (diminish #'editorconfig-mode))

(provide 'pg-editing)
