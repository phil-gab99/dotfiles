;;; pg-programming-java.el -*- lexical-binding: t; -*-
;; Author: Philippe Gabriel

(defun pg/spring-boot-properties ()
  "Makes appropriate calls when opening a spring properties file."
  (when (not (equal nil (string-match-p "application\\(-?[^-]+\\)?\\.properties"
                                        (file-name-nondirectory (buffer-file-name)))))
    (progn (run-hooks 'prog-mode-hook)
           (lsp-deferred))))

(use-package lsp-java
  :straight t
  :init
  (require 'lsp-java)
  (dolist (feature '(dap-java
                     lsp-java-boot))
    (require feature))
  :hook
  (java-mode . (lsp-deferred lsp-java-boot-lens-mode))
  (find-file . pg/spring-boot-properties)
  :custom
  (lsp-java-configuration-runtimes '[( :name "JavaSE-17"
                                       :path (expand-file-name "~/.guix-extra-profiles/java/java")
                                       :default t)])
  (lsp-java-vmargs (list "-noverify" "--enable-preview"))
  (lsp-java-java-path "java")
  (lsp-java-import-gradle-java-home (expand-file-name "~/.guix-extra-profiles/java/java"))
  :bind
  (:map lsp-mode-map
        ("C-<return>" . lsp-execute-code-action)))

(provide 'pg-programming-java)
