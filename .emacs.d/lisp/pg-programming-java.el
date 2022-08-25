(defun spring-boot-properties ()
  "Makes appropriate calls when opening a spring properties file."
  (when (not (equal nil (string-match-p "application\\(-?[^-]+\\)?\\.properties"
                                        (file-name-nondirectory (buffer-file-name)))))
    (progn (run-hooks 'prog-mode-hook)
           (lsp-deferred))))

(with-eval-after-load 'lsp-mode
  (require 'lsp-java)
  (with-eval-after-load 'lsp-java
    (bind-key "C-<return>" #'lsp-execute-code-action lsp-mode-map)
    (require 'dap-java)
    (require 'lsp-java-boot)
    (add-hook 'java-mode-hook #'lsp-deferred)
    (add-hook 'java-mode-hook #'lsp-java-boot-lens-mode)
    (add-hook 'find-file-hook #'spring-boot-properties)
    (customize-set-variable 'lsp-java-configuration-runtimes '[( :name "JavaSE-17"
                                                                 :path "~/.guix-extra-profiles/java/java"
                                                                 :default t)])
    (customize-set-variable 'lsp-java-vmargs (list "-noverify" "--enable-preview"))
    (customize-set-variable 'lsp-java-java-path "java")
    (customize-set-variable 'lsp-java-import-gradle-java-home "~/.guix-extra-profiles/java/java")))

(provide 'pg-programming-java)
