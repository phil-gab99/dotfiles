;;; pg-programming-lmc.el -*- lexical-binding: t; -*-
;; Author: Philippe Gabriel

(defvar lmc-java-mode-hook nil)

(add-to-list 'auto-mode-alist '("\\.lmc\\'" . lmc-java-mode))

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
  :syntax-table lmc-java-mode-syntax-table

  (set (make-local-variable 'font-lock-defaults) '(lmc-java-font-lock-defaults))

  (setq-local comment-start "# "
              comment-end ""
              indent-tabs-mode nil))

(provide 'pg-programming-lmc)
