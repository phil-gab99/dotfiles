(use-package sql
  :straight nil
  :hook
  (sql-interactive-mode . (lambda ()
                            (toggle-truncate-lines t)))
  :custom
  (sql-connection-alist '((main (sql-product 'postgres)
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

(use-package lsp-sqls
  :straight nil
  :after (sql lsp-mode)
  :hook
  (sql-mode . lsp-deferred)
  :custom
  (lsp-sqls-connections
   `(,(cl-pairlis '(driver dataSourceName)
                  `(("postgresql") ,(concat "host=127.0.0.1 port=5432 user=phil-gab99 password="
                                            (pg/lookup-password :host "localhost" :user "phil-gab99" :port 5432)
                                            " dbname=phil-gab99 sslmode=disable")))
     ,(cl-pairlis '(driver dataSourceName)
                  `(("postgresql") ,(concat "host=127.0.0.1 port=5432 user=phil-gab99 password="
                                            (pg/lookup-password :host "localhost" :user "phil-gab99" :port 5432)
                                            " dbname=ift2935 sslmode=disable"))))))

(use-package sql-indent
  :straight t
  :after sql
  :hook
  (sql-mode . sqlind-minor-mode)
  :config
  (setq-default sqlind-basic-offset 4))

(provide 'pg-programming-sql)
