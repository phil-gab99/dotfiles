;;; pg-programming-sql.el -*- lexical-binding: t; -*-
;; Author: Philippe Gabriel

(with-eval-after-load 'sql
  (add-hook 'sql-interactive-mode-hook #'(lambda ()
                                           (toggle-truncate-lines t)))
  (customize-set-variable 'sql-connection-alist
                          '((main (sql-product 'postgres)
                                  (sql-port 5432)
                                  (sql-server "localhost")
                                  (sql-user "phil-gab99")
                                  (sql-password (pg/lookup-password :host "localhost" :user "phil-gab99" :port 5432))
                                  (sql-database "phil-gab99"))
                            (school (sql-product 'postgres)
                                    (sql-port 5432)
                                    (sql-server "localhost")
                                    (sql-user "phil-gab99")
                                    (sql-password (pg/lookup-password :host "localhost" :user "phil-gab99" :port 5432))
                                    (sql-database "ift2935")))))

(with-eval-after-load 'sql
  (with-eval-after-load 'lsp-mode
    (require 'lsp-sqls)))
(with-eval-after-load 'lsp-sqls
  (add-hook 'sql-mode-hook #'lsp-deferred)
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
  (add-hook 'sql-mode-hook #'sqlind-minor-mode)
  (setq-default sqlind-basic-offset 4))

(provide 'pg-programming-sql)
