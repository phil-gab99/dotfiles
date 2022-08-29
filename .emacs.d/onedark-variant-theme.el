;;; onedark-variant-theme.el --- One Dark Variant color theme

;; Author: Philippe Gabriel

(deftheme onedark-variant
  "One Dark Variant - My onedark variant custom theme.")

(defvar onedark-variant-colors-alist
  '(("onedark-variant-whitestd"   . "#cccccc")
    ("onedark-variant-white"      . "#ffffff")
    ("onedark-variant-gold"       . "#eead0e")
    ("onedark-variant-black"      . "#282c34")
    ("onedark-variant-dark"       . "#000000")
    ("onedark-variant-doc-box"    . "#1a1a1a")
    ("onedark-variant-cursorbg"   . "#4682d9")
    ("onedark-variant-regionbg"   . "#494949")
    ("onedark-variant-regionds"   . "#828997")
    ("onedark-variant-blue"       . "#00bfff")
    ("onedark-variant-green"      . "#2cff64")
    ("onedark-variant-keywords"   . "#c678dd")
    ("onedark-variant-strings"    . "#98c379")
    ("onedark-variant-docs"       . "#98f979")
    ("onedark-variant-modebox"    . "#181a1f")
    ("onedark-variant-comments"   . "#737373")
    ("onedark-variant-types"      . "#ecbe7b")
    ("onedark-variant-vars"       . "#ff6347")
    ("onedark-variant-link"       . "#61afef")
    ("onedark-variant-constants"  . "#eead0e")
    ("onedark-variant-tags"       . "#00eeee")
    ("onedark-variant-builtin"    . "#00cdcd")
    ("onedark-variant-preproc"    . "#7b447b")
    ("onedark-variant-shexec"     . "#afeeee")
    ("onedark-variant-modelnfg"   . "#9da5b4")
    ("onedark-variant-modelnbg"   . "#21252b")
    ("onedark-variant-modelndiff" . "#ff4c35")
    ("onedark-variant-modelninfg" . "#5b6268")
    ("onedark-variant-modelninbg" . "#21242b")
    ("onedark-variant-lnnumfg"    . "#8f8f8f")
    ("onedark-variant-lnnumbg"    . "#202231")
    ("onedark-variant-lnsltfg"    . "#fefefe")
    ("onedark-variant-mu4eheader" . "#ff83fa")
    ("onedark-variant-parenmatch" . "#c678dd")
    ("onedark-variant-error"      . "#ff6666")
    ("onedark-variant-warn"       . "#f2db05")
    ("onedark-variant-minibuf"    . "#f0fff0")
    ("onedark-variant-eviln"      . "#818ff4"))
  "List of One Dark Variant colors.")

(defmacro onedark-variant-with-color-variables (&rest body)
  "Bind the colors list around BODY."
  (declare (indent 0))
  `(let ((class '((class color) (min-colors 89)))
         ,@ (mapcar (lambda (cons)
                      (list (intern (car cons)) (cdr cons)))
                    onedark-variant-colors-alist))
     ,@body))

(onedark-variant-with-color-variables
 (custom-theme-set-faces
  'onedark-variant

  ;; General
  `(cursor                   ((t ( :background ,onedark-variant-cursorbg))))
  `(default                  ((t ( :foreground ,onedark-variant-whitestd
                                   :background ,onedark-variant-black))))
  `(link                     ((t ( :foreground ,onedark-variant-link
                                   :weight bold
                                   :underline t))))
  `(region                   ((t ( :background ,onedark-variant-regionbg
                                   :distant-foreground ,onedark-variant-regionds))))
  `(highlight                ((t ( :background ,onedark-variant-regionbg
                                   :distant-foreground ,onedark-variant-regionds))))
  `(show-paren-match         ((t ( :foreground ,onedark-variant-parenmatch
                                   :weight bold))))
  `(sh-quoted-exec           ((t ( :foreground ,onedark-variant-shexec))))
  `(line-number              ((t ( :foreground ,onedark-variant-lnnumfg
                                   :background ,onedark-variant-lnnumbg))))
  `(line-number-current-line ((t ( :foreground ,onedark-variant-lnsltfg
                                   :background ,onedark-variant-lnnumbg))))
  `(minibuffer-prompt        ((t ( :foreground ,onedark-variant-minibuf))))
  `(shadow                   ((t ( :foreground ,onedark-variant-white))))
  `(error                    ((t ( :foreground ,onedark-variant-error))))
  `(warning                  ((t ( :foreground ,onedark-variant-warn))))
  `(message-header-name      ((t ( :bold t))))

  ;; Persp
  `(persp-selected-face ((t ( :foreground ,onedark-variant-warn))))

  ;; Man
  `(Man-overstrike ((t ( :foreground ,onedark-variant-types
                         :weight bold))))
  `(Man-underline  ((t ( :foreground ,onedark-variant-keywords
                         :underline t))))

  ;; Org
  `(org-hide                  ((t ( :foreground ,onedark-variant-black))))
  `(org-level-4               ((t ( :foreground ,onedark-variant-green))))
  `(org-date                  ((t ( :foreground ,onedark-variant-blue))))
  `(org-document-info-keyword ((t ( :inherit (font-lock-comment-face
                                              fixed-pitch)))))
  `(org-document-title        ((t ( :foreground ,onedark-variant-whitestd
                                    :weight bold))))
  `(org-document-info         ((t ( :foreground ,onedark-variant-whitestd))))

  ;; Outshine
  `(outshine-level-4 ((t ( :foreground ,onedark-variant-green))))

  ;; Sp
  `(sp-pair-overlay-face ((t ( :background ,onedark-variant-regionbg))))

  ;; Company
  `(company-tooltip-selection  ((t ( :background ,onedark-variant-regionbg
                                     :distant-foreground ,onedark-variant-regionds))))
  `(company-tooltip-mouse      ((t ( :background ,onedark-variant-regionbg
                                     :distant-foreground ,onedark-variant-regionds))))
  `(company-tooltip-annotation ((t ( :foreground ,onedark-variant-gold))))
  `(company-box-scrollbar      ((t ( :background ,onedark-variant-regionbg))))
  `(company-tooltip            ((t ( :foreground ,onedark-variant-whitestd 
                                     :background ,onedark-variant-doc-box))))

  ;; Mode line
  `(mode-line                       ((t ( :foreground ,onedark-variant-whitestd
                                          :background ,onedark-variant-modelnbg
                                          :box ,onedark-variant-modebox))))
  `(mode-line-inactive              ((t ( :foreground ,onedark-variant-modelninfg
                                          :background ,onedark-variant-modelninbg))))
  `(doom-modeline-buffer-modified   ((t ( :foreground ,onedark-variant-modelndiff))))
  `(doom-modeline-urgent            ((t ( :foreground ,onedark-variant-error))))
  `(doom-modeline-lsp-success       ((t ( :foreground ,onedark-variant-docs))))
  `(doom-modeline-buffer-major-mode ((t ( :foreground ,onedark-variant-blue))))
  `(doom-modeline-evil-normal-state ((t ( :foreground ,onedark-variant-eviln
                                          :weight bold))))

  ;; mu4e
  `(mu4e-header-key-face ((t ( :foreground ,onedark-variant-mu4eheader))))

  ;; eshell
  `(eshell-prompt ((t ( :foreground ,onedark-variant-gold))))

  ;; Ledger
  `(ledger-font-payee-cleared-face   ((t ( :foreground ,onedark-variant-green))))
  `(ledger-font-posting-account-face ((t ( :inherit (font-lock-builtin-face)))))

  ;; lsp
  `(lsp-lsp-flycheck-warning-unnecessary-face ((t ( :inherit (flycheck-warning)))))

  ;; lsp-ui
  `(lsp-ui-doc-background ((t ( :background ,onedark-variant-doc-box))))

  ;; Flycheck
  `(flycheck-warning ((t ( :underline ( :color ,onedark-variant-warn
                                        :style wave)))))

  ;; Tab bar
  `(tab-bar              ((t ( :foreground ,onedark-variant-whitestd
                               :background ,onedark-variant-modelnbg))))
  `(tab-bar-tab          ((t ( :foreground ,onedark-variant-white
                               :box ( :line-width -1
                                      :color ,onedark-variant-modelnbg
                                      :style pressed-button)))))
  `(tab-bar-tab-inactive ((t ( :foreground ,onedark-variant-whitestd
                               :background ,onedark-variant-modelnbg))))

  ;; Markdown
  `(markdown-code-face ((t ( :background ,onedark-variant-doc-box))))

  ;; nusmv
  `(nusmv-font-lock-variable-name-face ((t ( :inherit (font-lock-variable-name-face)))))
  `(nusmv-font-lock-keyword-face       ((t ( :inherit (font-lock-keyword-face)))))
  `(nusmv-font-lock-constant-face      ((t ( :inherit (font-lock-constant-face)))))
  `(nusmv-font-lock-type-face          ((t ( :inherit (font-lock-type-face)))))
  `(nusmv-font-lock-module-name-face   ((t ( :inherit (font-lock-function-name-face)))))
  `(nusmv-font-lock-directive-face     ((t ( :inherit (font-lock-preprocessor-face)))))
  `(nusmv-font-lock-separator-face     ((t ( :inherit (font-lock-builtin-face)))))

  ;; vterm
  `(which-func ((t ( :foreground ,onedark-variant-blue))))

  ;; sudoku
  `(sudoku-value-face ((t ( :foreground ,onedark-variant-mu4eheader
                            :height 2.0))))

  ;; Makefile
  `(makefile-space ((t ( :background ,onedark-variant-black))))

  ;; slack
  `(slack-message-output-text    ((t ( :inherit (variable-pitch)))))
  `(slack-mrkdwn-code-face       ((t ( :inherit (org-code)))))
  `(slack-mrkdwn-code-block-face ((t ( :inherit (org-verbatim)))))

  ;; General font locks
  `(font-lock-function-name-face ((t ( :foreground ,onedark-variant-blue))))
  `(font-lock-preprocessor-face  ((t ( :foreground ,onedark-variant-preproc))))
  `(font-lock-keyword-face       ((t ( :foreground ,onedark-variant-keywords))))
  `(font-lock-string-face        ((t ( :foreground ,onedark-variant-strings))))
  `(font-lock-doc-face           ((t ( :foreground ,onedark-variant-docs
                                       :slant italic))))
  `(font-lock-comment-face       ((t ( :foreground ,onedark-variant-comments
                                       :slant italic))))
  `(font-lock-type-face          ((t ( :foreground ,onedark-variant-types))))
  `(font-lock-variable-name-face ((t ( :foreground ,onedark-variant-vars))))
  `(font-lock-constant-face      ((t ( :foreground ,onedark-variant-constants))))
  `(font-lock-doc-markup-face    ((t ( :foreground ,onedark-variant-tags))))
  `(font-lock-builtin-face       ((t ( :foreground ,onedark-variant-builtin))))))

(provide-theme 'onedark-variant)
