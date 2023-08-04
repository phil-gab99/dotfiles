;;; pg-programming-sql.el -*- lexical-binding: t; -*-
;; Author: Philippe Gabriel

(unless (fboundp 'lsp-deferred)
  (autoload #'lsp-deferred "lsp-mode" nil t))
(add-hook 'sql-mode-hook #'lsp-deferred)
(unless (fboundp 'sqlind-minor-mode)
  (autoload #'sqlind-minor-mode "sql-indent" nil t))
(add-hook 'sql-mode-hook #'sqlind-minor-mode)
(add-hook 'sql-interactive-mode-hook #'(lambda ()
                                         (toggle-truncate-lines t)))
(with-eval-after-load 'sql
  (customize-set-variable 'sql-connection-alist
                          `((main (sql-product postgres)
                                  (sql-port 5432)
                                  (sql-server "localhost")
                                  (sql-user "phil-gab99")
                                  (sql-password ,(pg/lookup-password :host "localhost" :user "phil-gab99" :port 5432))
                                  (sql-database "phil-gab99"))
                            (school (sql-product postgres)
                                    (sql-port 5432)
                                    (sql-server "localhost")
                                    (sql-user "phil-gab99")
                                    (sql-password ,(pg/lookup-password :host "localhost" :user "phil-gab99" :port 5432))
                                    (sql-database "ift2935")))))

(with-eval-after-load 'sql
  (with-eval-after-load 'lsp-mode
    (require 'lsp-sqls)))
(with-eval-after-load 'lsp-sqls
  (customize-set-variable 'lsp-sqls-connections
                          `(,(cl-pairlis '(driver dataSourceName)
                                         `(("postgresql") ,(concat "host=127.0.0.1 port=5432 user=phil-gab99 password="
                                                                   (pg/lookup-password :host "localhost"
                                                                                       :user "phil-gab99"
                                                                                       :port 5432)
                                                                   " dbname=phil-gab99 sslmode=disable")))
                            ,(cl-pairlis '(driver dataSourceName)
                                         `(("postgresql") ,(concat "host=127.0.0.1 port=5432 user=phil-gab99 password="
                                                                   (pg/lookup-password :host "localhost"
                                                                                       :user "phil-gab99"
                                                                                       :port 5432)
                                                                   " dbname=ift2935 sslmode=disable"))))))

(straight-use-package 'sql-indent)
(with-eval-after-load 'sql
  (require 'sql-indent))
(with-eval-after-load 'sql-indent
  (setq-default sqlind-basic-offset 4))

(provide 'pg-programming-sql)
