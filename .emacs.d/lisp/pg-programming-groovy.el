;;; pg-programming-groovy.el -*- lexical-binding: t; -*-
;; Author: Philippe Gabriel

(straight-use-package '(groovy-emacs-modes :type git
                                           :host github
                                           :repo "Groovy-Emacs-Modes/groovy-emacs-modes"))

(add-to-list 'safe-local-variable-values
             '(eval . (setq lsp-groovy-classpath (seq-concatenate
                                                   #'vector
                                                   (mapcar #'file-name-parent-directory
                                                           (file-expand-wildcards
                                                            (concat
                                                             (getenv "GRADLE_HOME")
                                                             "/caches/modules-2/files-2.1/*/*/*/*/*.jar")))
                                                   (file-expand-wildcards
                                                    (concat
                                                     (getenv "GRADLE_HOME")
                                                     "/wrapper/dists/*/*/*/lib"))
                                                   (file-expand-wildcards
                                                    (concat
                                                     (getenv "GRADLE_HOME")
                                                     "/wrapper/dists/*/*/*/lib/plugins"))
                                                   (file-expand-wildcards
                                                    (concat
                                                     (getenv "GRADLE_HOME")
                                                     "/wrapper/dists/*/*/*/lib/agents"))))))

(add-hook 'groovy-mode-hook #'lsp-deferred)

(with-eval-after-load 'groovy-mode
  (setopt groovy-indent-offset 2))

(provide 'pg-programming-groovy)
