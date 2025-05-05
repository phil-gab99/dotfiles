;;; pg-programming-java.el -*- lexical-binding: t; -*-
;; Author: Philippe Gabriel

(defun pg/spring-boot-properties ()
    "Makes appropriate calls when opening a spring properties file."
    (when (not (equal nil (string-match-p "application\\(-?[^-]+\\)?\\.properties"
                                          (file-name-nondirectory (buffer-file-name)))))
      (progn (run-hooks 'prog-mode-hook)
             (lsp-deferred))))

  (defun pg/generate-serial-version-uid ()
    "Generates the serialVersionUID for a class implementing
java.lang.Serializable. Relies on a WORKSPACE environment variable which points
to the root of a java project. Also relies on the use of `lsp-java' for
generating class files of visited files."
    (interactive)
    (let* ((file-path (buffer-file-name)) 
           (class-name (s-replace-all
                       '(("/" . "."))
                       (substring (f-no-ext file-path)
                                  (+ 5 (s-index-of "java" file-path)))))
           (class-dir (s-prepend (getenv "WORKSPACE") "/bin/"))
           (class-path (s-split
                        "\n"
                        (s-trim (shell-command-to-string (s-prepend "ls "
                                                                    class-dir)))))
           (serial-cmd (shell-command-to-string
                        (concat "serialver -classpath \""
                                (s-join ":" (mapcar (lambda (target)
                                                      (s-prepend class-dir target))
                                                    class-path))
                                "\" "
                                class-name)))
           (serializablep (s-index-of "private" serial-cmd)))
      (if (not serializablep)
          (message serial-cmd)
        (with-current-buffer (current-buffer)
          (save-excursion
            (beginning-of-buffer)
            (or (search-forward "class" nil t)
                (search-forward "interface" nil t)
                (search-forward "record" nil t))
            (forward-line 1)
            (insert (substring serial-cmd serializablep))
            (forward-line -1)
            (company-indent-or-complete-common (point)))))))

  (add-to-list 'safe-local-variable-values
               '(eval . (setq lsp-java-import-gradle-user-home (getenv "GRADLE_USER_HOME")
                              lsp-java-import-gradle-home (getenv "GRADLE_HOME")
                              lsp-java-import-gradle-java-home (getenv "JAVA_HOME")
                              lsp-java-java-path (concat (getenv "JAVA_HOME") "/bin/java")
                              lsp-java-configuration-runtimes `[( :name "JavaSE-17"
                                                                  :path ,(getenv "JAVA_HOME")
                                                                  :default t)]
                              lsp-java-vmargs (list "-noverify"
                                                    "-Xmx1G"
                                                    "-XX:+UseG1GC"
                                                    "-XX:+UseStringDeduplication"
                                                    "--enable-preview"
                                                    (concat
                                                     "-javaagent:"
                                                     (car (file-expand-wildcards
                                                           (concat
                                                            (getenv "GRADLE_HOME")
                                                            "/caches/modules-2/files-2\.1/org\.projectlombok/lombok/[0-9\.]+/[a-zA-Z0-9]+/lombok-[0-9\.]+\.jar")
                                                           nil t)))))))

  (straight-use-package 'lsp-java)
  (unless (fboundp 'lsp-deferred)
    (autoload #'lsp-deferred "lsp-mode" nil t))
  (add-hook 'java-ts-mode-hook #'lsp-deferred)
  (setq lsp-java-compile-null-analysis-mode "automatic")
  (setopt lsp-java-boot-enabled nil)
  ;; (unless (fboundp 'lsp-java-boot-lens-mode)
  ;;   (autoload #'lsp-java-boot-lens-mode "lsp-java-boot" nil t))
  ;; (add-hook 'java-ts-mode-hook #'lsp-java-boot-lens-mode)

  (with-eval-after-load 'lsp-mode
    (require 'lsp-java))
  (with-eval-after-load 'lsp-java
    (add-hook 'find-file-hook #'pg/spring-boot-properties)
    (dolist (feature '(helm
                       dap-java
                       lsp-java-boot))
      (require feature))
    (define-key lsp-mode-map (kbd "M-<return>") #'lsp-execute-code-action)
    (setopt lsp-java-completion-import-order ["java" "javax" "jakarta" "org" "io" "lombok" "com"]
            lsp-java-completion-favorite-static-members ["org.junit.Assert.*"
                                                         "org.junit.Assume.*"
                                                         "org.junit.jupiter.api.Assertions.*"
                                                         "org.junit.jupiter.api.Assumptions.*"
                                                         "org.junit.jupiter.api.DynamicContainer.*"
                                                         "org.junit.jupiter.api.DynamicTest.*"
                                                         "org.mockito.Mockito.*"
                                                         "org.mockito.ArgumentMatchers.*"
                                                         "org.mockito.Answers.*"
                                                         "org.hamcrest.Matchers.*"
                                                         "org.springframework.test.web.servlet.result.MockMvcResultMatchers.*"
                                                         "org.springframework.test.web.servlet.result.MockMvcResultHandlers.*"
                                                         "org.springframework.test.web.servlet.request.MockMvcRequestBuilders.*"]))

(provide 'pg-programming-java)
