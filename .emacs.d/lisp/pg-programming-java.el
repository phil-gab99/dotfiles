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
  (setopt lsp-java-configuration-runtimes `[( :name "JavaSE-17"
                                              :path ,(getenv "JAVA_HOME")
                                              :default t)]
          lsp-java-vmargs (list "-noverify" "-Xmx1G" "-XX:+UseG1GC" "-XX:+UseStringDeduplication" "--enable-preview")
          lsp-java-java-path "java"
          lsp-java-import-gradle-java-home (getenv "JAVA_HOME")))

(provide 'pg-programming-java)
