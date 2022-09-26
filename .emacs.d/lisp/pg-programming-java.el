;;; pg-programming-java.el -*- lexical-binding: t; -*-
;; Author: Philippe Gabriel

(defun pg/spring-boot-properties ()
  "Makes appropriate calls when opening a spring properties file."
  (when (not (equal nil (string-match-p "application\\(-?[^-]+\\)?\\.properties"
                                        (file-name-nondirectory (buffer-file-name)))))
    (progn (run-hooks 'prog-mode-hook)
           (lsp-deferred))))

(straight-use-package 'lsp-java)
(with-eval-after-load 'lsp-mode
  (require 'lsp-java))
(with-eval-after-load 'lsp-java
  (dolist (feature '(dap-java
                     lsp-java-boot))
    (require feature))
  (add-hook 'java-mode-hook #'lsp-deferred)
  (add-hook 'find-file-hook #'pg/spring-boot-properties)
  (define-key lsp-mode-map (kbd "C-<return>") #'lsp-execute-code-action)
  (pg/customize-set-variables
   `((lsp-java-configuration-runtimes . [( :name "JavaSE-17"
                                           :path ,(concat (getenv "GUIX_EXTRA_PROFILES") "/java/java")
                                           :default t)])
     (lsp-java-vmargs . ,(list "-noverify" "--enable-preview"))
     (lsp-java-java-path . "java")
     (lsp-java-import-gradle-java-home . ,(concat (getenv "GUIX_EXTRA_PROFILES") "/java/java")))))
(with-eval-after-load 'lsp-java-boot
  (add-hook 'java-mode-hook #'lsp-java-boot-lens-mode))

(provide 'pg-programming-java)
