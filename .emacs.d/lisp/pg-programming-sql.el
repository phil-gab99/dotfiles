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
  (setopt sql-connection-alist
          `((main (sql-product postgres)
                  (sql-port 5432)
                  (sql-server "localhost")
                  (sql-user (plist-get pg/user :user))
                  (sql-password ,(pg/lookup-password :host "localhost" :user (plist-get pg/user :user) :port 5432))
                  (sql-database (plist-get pg/user :user)))
            (school (sql-product postgres)
                    (sql-port 5432)
                    (sql-server "localhost")
                    (sql-user (plist-get pg/user :user))
                    (sql-password ,(pg/lookup-password :host "localhost" :user (plist-get pg/user :user) :port 5432))
                    (sql-database "ift2935")))))

(with-eval-after-load 'sql
  (with-eval-after-load 'lsp-mode
    (require 'lsp-sqls)))
(with-eval-after-load 'lsp-sqls
  (setopt lsp-sqls-connections
          `(,(cl-pairlis '(driver dataSourceName)
                         `("postgresql" ,(concat "host=127.0.0.1 port=5432 user="
                                                 (plist-get pg/user :user)
                                                 "password="
                                                 (pg/lookup-password :host "localhost"
                                                                     :user (plist-get pg/user :user)
                                                                     :port 5432)
                                                 " dbname="(plist-get pg/user :user)
                                                 " sslmode=disable")))
            ,(cl-pairlis '(driver dataSourceName)
                         `("postgresql" ,(concat "host=127.0.0.1 port=5432 user="
                                                 (plist-get pg/user :user)
                                                 "password="
                                                 (pg/lookup-password :host "localhost"
                                                                     :user (plist-get pg/user :user)
                                                                     :port 5432)
                                                 " dbname=ift2935 sslmode=disable"))))))

(straight-use-package 'sql-indent)
(with-eval-after-load 'sql
  (require 'sql-indent))
(with-eval-after-load 'sql-indent
  (setq-default sqlind-basic-offset 4))

(provide 'pg-programming-sql)
