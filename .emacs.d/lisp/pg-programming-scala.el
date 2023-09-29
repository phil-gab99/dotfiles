;;; pg-programming-scala.el -*- lexical-binding: t; -*-
;; Author: Philippe Gabriel

(straight-use-package 'scala-mode)

(unless (fboundp 'scala-mode)
  (autoload #'scala-mode "scala-mode" nil t))
(add-to-list 'interpreter-mode-alist '("scala" . scala-mode))

(straight-use-package 'sbt-mode)
(unless (fboundp 'sbt-start)
  (autoload #'sbt-start "sbt-mode" nil t))
(unless (fboundp 'sbt-command)
  (autoload #'sbt-command "sbt-mode" nil t))
(with-eval-after-load 'sbt-mode
  (substitute-key-definition 'minibuffer-complete-word
                             'self-insert-command
                             minibuffer-local-completion-map))

(straight-use-package 'lsp-metals)

(pg/customize-set-variables
 `((lsp-metals-server-args . ("-J-Dmetals.allow-multiline-string-formatting=off" "-J-Dmetals.icons=unicode"))
   (lsp-metals-enable-semantic-highlighting . t)))
(unless (fboundp 'lsp)
  (autoload #'lsp-deferred "lsp-mode"))
(add-hook 'scala-mode-hook #'lsp)

(provide 'pg-programming-scala)
