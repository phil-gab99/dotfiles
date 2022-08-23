(require 'lsp-sqls)
(use-package sql
   :straight nil
   :hook (sql-mode . lsp-deferred)
   :config
   (add-hook 'sql-interactive-mode-hook (lambda () (toggle-truncate-lines t)))
   :custom
   ;; (sql-postgres-login-params '((user :default "phil-gab99")
   ;;                              (database :default "phil-gab99")
   ;;                              (server :default "localhost")
   ;;                              (port :default 5432)))

   (sql-connection-alist
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
              (sql-database "ift2935"))))

   (lsp-sqls-server "~/go/bin/sqls")
   (setq lsp-sqls-connections
         (list
          (list
           (cl-pairlis '(driver dataSourceName)
                       (list '("postgresql") (concat "host=127.0.0.1 port=5432 user=phil-gab99 password="
                                                     (pg/lookup-password :host "localhost" :user "phil-gab99" :port 5432)
                                                     " dbname=phil-gab99 sslmode=disable")))
           (cl-pairlis '(driver dataSourceName)
                       (list '("postgresql") (concat "host=127.0.0.1 port=5432 user=phil-gab99 password="
                                                     (pg/lookup-password :host "localhost" :user "phil-gab99" :port 5432)
                                                     " dbname=ift2935 sslmode=disable")))))))

(use-package sql-indent
  :straight t
  :hook (sql-mode . sqlind-minor-mode)
  :config
  (setq-default sqlind-basic-offset 4))

(provide 'pg-programming-sql)
