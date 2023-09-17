;;; pg-programming-java.el -*- lexical-binding: t; -*-
;; Author: Philippe Gabriel

(defun pg/spring-boot-properties ()
  "Makes appropriate calls when opening a spring properties file."
  (when (not (equal nil (string-match-p "application\\(-?[^-]+\\)?\\.properties"
                                        (file-name-nondirectory (buffer-file-name)))))
    (progn (run-hooks 'prog-mode-hook)
           (lsp-deferred))))

(straight-use-package 'lsp-java)
(unless (fboundp 'lsp-deferred)
  (autoload #'lsp-deferred "lsp-mode" nil t))
(add-hook 'java-mode-hook #'lsp-deferred)
(unless (fboundp 'lsp-java-boot-lens-mode)
  (autoload #'lsp-java-boot-lens-mode "lsp-java-boot" nil t))
(add-hook 'java-mode-hook #'lsp-java-boot-lens-mode)
(with-eval-after-load 'lsp-mode
  (require 'lsp-java))
(with-eval-after-load 'lsp-java
  (add-hook 'find-file-hook #'pg/spring-boot-properties)
  (dolist (feature '(dap-java
                     lsp-java-boot))
    (require feature))
  (define-key lsp-mode-map (kbd "C-<return>") #'lsp-execute-code-action)
  (pg/customize-set-variables
   `((lsp-java-configuration-runtimes . [( :name "JavaSE-17"
                                           :path ,(concat (getenv "GUIX_EXTRA_PROFILES") "/java/java")
                                           :default t)])
     (lsp-java-vmargs . ,(list "--noverify" "--enable-preview"))
     (lsp-java-java-path . "java")
     (lsp-java-import-gradle-java-home . ,(concat (getenv "GUIX_EXTRA_PROFILES") "/java/java")))))

(provide 'pg-programming-java)
