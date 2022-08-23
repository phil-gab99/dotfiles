(defun spring-boot-properties ()
  "Makes appropriate calls when opening a spring properties file"
  (when (not (equal nil (string-match-p "application\\(-?[^-]+\\)?\\.properties"
                                        (file-name-nondirectory (buffer-file-name)))))
    (progn (run-hooks 'prog-mode-hook)
           (lsp-deferred))))

(use-package lsp-java
  :straight t
  :hook (java-mode . lsp-deferred)
  :bind
  (:map lsp-mode-map
        ("C-<return>" . lsp-execute-code-action))
  :config
  (require 'dap-java)
  (require 'lsp-java-boot)
  (add-hook 'java-mode-hook #'lsp-java-boot-lens-mode)
  (add-hook 'find-file-hook #'spring-boot-properties)
  :custom
  (lsp-java-configuration-runtimes '[( :name "JavaSE-17"
                                       :path "~/.guix-extra-profiles/java/java"
                                       :default t)])
  (lsp-java-vmargs (list "-noverify" "--enable-preview"))
  (lsp-java-java-path "java")
  (lsp-java-import-gradle-java-home "~/.guix-extra-profiles/java/java"))

(defun pg/gradle-run ()
  "Execute gradle run command"
  (interactive)
  (gradle-run "run"))

(use-package gradle-mode
  :straight '(emacs-gradle-mode
              :host github
              :repo "jacobono/emacs-gradle-mode")
  :disabled ;; No gradle package on guix yet
  :hook (java-mode . gradle-mode))

(provide 'pg-programming-java)
