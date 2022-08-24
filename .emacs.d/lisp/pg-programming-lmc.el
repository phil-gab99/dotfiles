(require 'pg-startup)

(defvar lmc-java-mode-hook nil)

;; (add-to-list 'auto-mode-alist '("\\.lmc\\'" . lmc-java-mode))

(defconst lmc-java-font-lock-defaults
  (list
   '("#.*" . font-lock-comment-face)
   '("\\<\\(ADD\\|BR[PZ]?\\|DAT\\|HLT\\|IN\\|LDA\\|OUT\\|S\\(?:TO\\|UB\\)\\)\\>" . font-lock-keyword-face)
   '("^\\w+" . font-lock-function-name-face)
   '("\\b[0-9]+\\b" . font-lock-constant-face))
  "Minimal highlighting expressions for lmc mode")

(defvar lmc-java-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?# ". 1b" st)
    (modify-syntax-entry ?\n "> b" st)
    st)
  "Syntax table for lmc-mode")

(define-derived-mode lmc-java-mode prog-mode "LMC"
  "Major mode for editing lmc files"
  :syntax-table lmc-mode-syntax-table

  (set (make-local-variable 'font-lock-defaults) '(lmc-font-lock-defaults))

  (setq-local comment-start "# ")
  (setq-local comment-end "")

  (setq-local indent-tabs-mode nil))

(define-derived-mode pg/lmc-asm-mode prog-mode "LMC-Asm"
  "Major mode to edit LMC assembly code."
  :syntax-table emacs-lisp-mode-syntax-table
  (set (make-local-variable 'font-lock-defaults)
       '(lmc-asm-font-lock-keywords))
  (set (make-local-variable 'indent-line-function)
       #'lmc-asm-indent-line)
  (set (make-local-variable 'indent-tabs-mode) nil)
  (set (make-local-variable 'imenu-generic-expression)
       lmc-asm-imenu-generic-expression)
  (set (make-local-variable 'outline-regexp) lmc-asm-outline-regexp)
  (add-hook 'completion-at-point-functions #'lmc-asm-completion nil t)
  (set (make-local-variable 'comment-start) "#")
  (set (make-local-variable 'comment-start-skip)
       "\\(\\(^\\|[^\\\\\n]\\)\\(\\\\\\\\\\)*\\)#+ *"))

(use-package lmc
  :straight t
  :config
  (fset #'lmc-asm-mode #'pg/lmc-asm-mode))

(provide 'pg-programming-lmc)
