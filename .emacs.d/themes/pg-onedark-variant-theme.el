;;; pg-onedark-variant-theme.el --- One Dark Variant color theme
;; Author: Philippe Gabriel

(deftheme pg-onedark-variant
  "One Dark Variant - My pg-onedark-variant custom theme.")

(defvar pg-onedark-variant-colors-alist
  '(("pg-onedark-variant-whitestd"    . "#cccccc")
    ("pg-onedark-variant-white"       . "#ffffff")
    ("pg-onedark-variant-gold"        . "#eead0e")
    ("pg-onedark-variant-black"       . "#282c34")
    ("pg-onedark-variant-dark"        . "#000000")
    ("pg-onedark-variant-doc-box"     . "#1a1a1a")
    ("pg-onedark-variant-cursorbg"    . "#4682d9")
    ("pg-onedark-variant-regionbg"    . "#494949")
    ("pg-onedark-variant-regionds"    . "#828997")
    ("pg-onedark-variant-blue"        . "#00bfff")
    ("pg-onedark-variant-green"       . "#2cff64")
    ("pg-onedark-variant-keywords"    . "#c678dd")
    ("pg-onedark-variant-strings"     . "#98c379")
    ("pg-onedark-variant-docs"        . "#98f979")
    ("pg-onedark-variant-modebox"     . "#181a1f")
    ("pg-onedark-variant-comments"    . "#737373")
    ("pg-onedark-variant-codeblockbg" . "#0d0d0d")
    ("pg-onedark-variant-quotefg"     . "#ffd700")
    ("pg-onedark-variant-code"        . "#ffa500")
    ("pg-onedark-variant-verbatim"    . "#00ff00")
    ("pg-onedark-variant-table"       . "#cdb5cd")
    ("pg-onedark-variant-types"       . "#ecbe7b")
    ("pg-onedark-variant-vars"        . "#ff6347")
    ("pg-onedark-variant-link"        . "#61afef")
    ("pg-onedark-variant-constants"   . "#eead0e")
    ("pg-onedark-variant-tags"        . "#00eeee")
    ("pg-onedark-variant-builtin"     . "#00cdcd")
    ("pg-onedark-variant-preproc"     . "#7b447b")
    ("pg-onedark-variant-shexec"      . "#afeeee")
    ("pg-onedark-variant-modelnfg"    . "#9da5b4")
    ("pg-onedark-variant-modelnbg"    . "#21252b")
    ("pg-onedark-variant-modelndiff"  . "#ff4c35")
    ("pg-onedark-variant-modelninfg"  . "#5b6268")
    ("pg-onedark-variant-modelninbg"  . "#21242b")
    ("pg-onedark-variant-lnnumfg"     . "#8f8f8f")
    ("pg-onedark-variant-lnnumbg"     . "#202231")
    ("pg-onedark-variant-lnsltfg"     . "#fefefe")
    ("pg-onedark-variant-mu4eheader"  . "#ff83fa")
    ("pg-onedark-variant-parenmatch"  . "#c678dd")
    ("pg-onedark-variant-error"       . "#ff6666")
    ("pg-onedark-variant-warn"        . "#f2db05")
    ("pg-onedark-variant-minibuf"     . "#f0fff0")
    ("pg-onedark-variant-eviln"       . "#818ff4"))
  "List of One Dark Variant colors.")

(defmacro pg-onedark-variant-with-color-variables (&rest body)
  "Bind the colors list around BODY."
  (declare (indent 0))
  `(let ((class '((class color) (min-colors 89)))
         ,@(mapcar (lambda (cons)
                     (list (intern (car cons)) (cdr cons)))
                   pg-onedark-variant-colors-alist))
     ,@body))

(pg-onedark-variant-with-color-variables
  (custom-theme-set-faces
   'pg-onedark-variant

   ;; General
   `(cursor                   ((t ( :background ,pg-onedark-variant-cursorbg))))
   `(default                  ((t ( :foreground ,pg-onedark-variant-whitestd
                                    :background ,pg-onedark-variant-black))))
   `(italic                   ((t ( :slant italic))))
   `(link                     ((t ( :foreground ,pg-onedark-variant-link
                                    :weight bold
                                    :underline t))))
   `(region                   ((t ( :background ,pg-onedark-variant-regionbg
                                    :distant-foreground ,pg-onedark-variant-regionds))))
   `(highlight                ((t ( :background ,pg-onedark-variant-regionbg
                                    :distant-foreground ,pg-onedark-variant-regionds))))
   `(show-paren-match         ((t ( :foreground ,pg-onedark-variant-parenmatch
                                    :weight bold))))
   `(sh-quoted-exec           ((t ( :foreground ,pg-onedark-variant-shexec))))
   `(line-number              ((t ( :foreground ,pg-onedark-variant-lnnumfg
                                    :background ,pg-onedark-variant-lnnumbg))))
   `(line-number-current-line ((t ( :foreground ,pg-onedark-variant-lnsltfg
                                    :background ,pg-onedark-variant-lnnumbg))))
   `(minibuffer-prompt        ((t ( :foreground ,pg-onedark-variant-minibuf))))
   `(shadow                   ((t ( :foreground ,pg-onedark-variant-white))))
   `(error                    ((t ( :foreground ,pg-onedark-variant-error))))
   `(warning                  ((t ( :foreground ,pg-onedark-variant-warn))))
   `(message-header-name      ((t ( :bold t))))

   ;; Persp
   `(persp-selected-face ((t ( :foreground ,pg-onedark-variant-warn))))

   ;; Man
   `(Man-overstrike ((t ( :foreground ,pg-onedark-variant-types
                          :weight bold))))
   `(Man-underline  ((t ( :foreground ,pg-onedark-variant-keywords
                          :underline t))))

   ;; Info
   `(Info-quoted    ((t ( :foreground ,pg-onedark-variant-code
                          :inherit (fixed-pitch)))))

   ;; Org
   `(org-hide                  ((t ( :foreground ,pg-onedark-variant-black))))
   `(org-level-4               ((t ( :foreground ,pg-onedark-variant-green))))
   `(org-date                  ((t ( :foreground ,pg-onedark-variant-blue))))
   `(org-quote                 ((t ( :slant italic
                                     :foreground ,pg-onedark-variant-quotefg
                                     :background ,pg-onedark-variant-codeblockbg
                                     :inherit (variable-pitch)))))
   `(org-document-info-keyword ((t ( :inherit (font-lock-comment-face
                                               fixed-pitch)))))
   `(org-document-title        ((t ( :foreground ,pg-onedark-variant-whitestd
                                     :weight bold))))
   `(org-document-info         ((t ( :foreground ,pg-onedark-variant-whitestd))))
   `(org-ellipsis              ((t ( :underline nil))))
   `(org-block                 ((t ( :foreground unspecified
                                     :background ,pg-onedark-variant-codeblockbg
                                     :inherit (fixed-pitch)))))
   `(org-code                  ((t ( :foreground ,pg-onedark-variant-code
                                     :inherit (fixed-pitch)))))
   `(org-verbatim              ((t ( :foreground ,pg-onedark-variant-verbatim
                                     :inherit (fixed-pitch)))))
   `(org-table                 ((t ( :foreground ,pg-onedark-variant-table
                                     :inherit (shaodw
                                               fixed-pitch)))))
   `(org-indent                ((t ( :inherit (org-hide
                                               fixed-pitch)))))
   `(org-special-keyword       ((t ( :inherit (font-lock-comment-face
                                               fixed-pitch)))))
   `(org-meta-line             ((t ( :inherit (font-lock-comment-face
                                               fixed-pitch)))))
   `(org-checkbox              ((t ( :inherit (fixed-pitch)))))

   ;; Outshine
   `(outshine-level-4 ((t ( :foreground ,pg-onedark-variant-green))))

   ;; Sp
   `(sp-pair-overlay-face ((t ( :background ,pg-onedark-variant-regionbg))))

   ;; Company
   `(company-tooltip-selection  ((t ( :background ,pg-onedark-variant-regionbg
                                      :distant-foreground ,pg-onedark-variant-regionds))))
   `(company-tooltip-mouse      ((t ( :background ,pg-onedark-variant-regionbg
                                      :distant-foreground ,pg-onedark-variant-regionds))))
   `(company-tooltip-annotation ((t ( :foreground ,pg-onedark-variant-gold))))
   `(company-box-scrollbar      ((t ( :background ,pg-onedark-variant-regionbg))))
   `(company-tooltip            ((t ( :foreground ,pg-onedark-variant-whitestd 
                                      :background ,pg-onedark-variant-doc-box))))

   ;; Mode line
   `(mode-line                       ((t ( :foreground ,pg-onedark-variant-whitestd
                                           :background ,pg-onedark-variant-modelnbg
                                           :box ,pg-onedark-variant-modebox))))
   `(mode-line-inactive              ((t ( :foreground ,pg-onedark-variant-modelninfg
                                           :background ,pg-onedark-variant-modelninbg))))
   `(doom-modeline-buffer-modified   ((t ( :foreground ,pg-onedark-variant-modelndiff))))
   `(doom-modeline-urgent            ((t ( :foreground ,pg-onedark-variant-error))))
   `(doom-modeline-lsp-success       ((t ( :foreground ,pg-onedark-variant-docs))))
   `(doom-modeline-buffer-major-mode ((t ( :foreground ,pg-onedark-variant-blue))))
   `(doom-modeline-evil-normal-state ((t ( :foreground ,pg-onedark-variant-eviln
                                           :weight bold))))

   ;; mu4e
   `(mu4e-header-key-face ((t ( :foreground ,pg-onedark-variant-mu4eheader))))

   ;; eshell
   `(eshell-prompt ((t ( :foreground ,pg-onedark-variant-gold))))

   ;; Ledger
   `(ledger-font-payee-cleared-face   ((t ( :foreground ,pg-onedark-variant-green))))
   `(ledger-font-posting-account-face ((t ( :inherit (font-lock-builtin-face)))))

   ;; lsp
   `(lsp-lsp-flycheck-warning-unnecessary-face ((t ( :inherit (flycheck-warning)))))

   ;; lsp-ui
   `(lsp-ui-doc-background ((t ( :background ,pg-onedark-variant-doc-box))))

   ;; Flycheck
   `(flycheck-warning ((t ( :underline ( :color ,pg-onedark-variant-warn
                                         :style wave)))))

   ;; Tab bar
   `(tab-bar              ((t ( :foreground ,pg-onedark-variant-whitestd
                                :background ,pg-onedark-variant-modelnbg))))
   `(tab-bar-tab          ((t ( :foreground ,pg-onedark-variant-white
                                :box ( :line-width -1
                                       :color ,pg-onedark-variant-modelnbg
                                       :style pressed-button)))))
   `(tab-bar-tab-inactive ((t ( :foreground ,pg-onedark-variant-whitestd
                                :background ,pg-onedark-variant-modelnbg))))

   ;; Markdown
   `(markdown-code-face ((t ( :background ,pg-onedark-variant-doc-box))))

   ;; nusmv
   `(nusmv-font-lock-variable-name-face ((t ( :inherit (font-lock-variable-name-face)))))
   `(nusmv-font-lock-keyword-face       ((t ( :inherit (font-lock-keyword-face)))))
   `(nusmv-font-lock-constant-face      ((t ( :inherit (font-lock-constant-face)))))
   `(nusmv-font-lock-type-face          ((t ( :inherit (font-lock-type-face)))))
   `(nusmv-font-lock-module-name-face   ((t ( :inherit (font-lock-function-name-face)))))
   `(nusmv-font-lock-directive-face     ((t ( :inherit (font-lock-preprocessor-face)))))
   `(nusmv-font-lock-separator-face     ((t ( :inherit (font-lock-builtin-face)))))

   ;; vterm
   `(which-func ((t ( :foreground ,pg-onedark-variant-blue))))

   ;; sudoku
   `(sudoku-value-face ((t ( :foreground ,pg-onedark-variant-mu4eheader
                             :height 2.0))))

   ;; Makefile
   `(makefile-space ((t ( :background ,pg-onedark-variant-black))))

   ;; slack
   `(slack-message-output-text    ((t ( :inherit (variable-pitch)))))
   `(slack-mrkdwn-code-face       ((t ( :inherit (org-code)))))
   `(slack-mrkdwn-code-block-face ((t ( :inherit (org-verbatim)))))

   ;; General font locks
   `(font-lock-function-name-face ((t ( :foreground ,pg-onedark-variant-blue))))
   `(font-lock-preprocessor-face  ((t ( :foreground ,pg-onedark-variant-preproc))))
   `(font-lock-keyword-face       ((t ( :foreground ,pg-onedark-variant-keywords))))
   `(font-lock-string-face        ((t ( :foreground ,pg-onedark-variant-strings))))
   `(font-lock-doc-face           ((t ( :foreground ,pg-onedark-variant-docs
                                        :slant italic))))
   `(font-lock-comment-face       ((t ( :foreground ,pg-onedark-variant-comments
                                        :slant italic))))
   `(font-lock-type-face          ((t ( :foreground ,pg-onedark-variant-types))))
   `(font-lock-variable-name-face ((t ( :foreground ,pg-onedark-variant-vars))))
   `(font-lock-constant-face      ((t ( :foreground ,pg-onedark-variant-constants))))
   `(font-lock-doc-markup-face    ((t ( :foreground ,pg-onedark-variant-tags))))
   `(font-lock-builtin-face       ((t ( :foreground ,pg-onedark-variant-builtin))))))

(provide-theme 'pg-onedark-variant)
