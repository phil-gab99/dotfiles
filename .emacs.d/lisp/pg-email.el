(require 'pg-startup)

(unless pg/is-termux
  (use-package mu4e
    :straight '( :type git
                 :host github
                 :repo "djcb/mu"
                 :branch "release/1.8")
                 ;(;:files ("mu4e/*")
                 ;:pre-build (("./autogen.sh") ("make") ("make install")))
    :commands mu4e
    :hook (mu4e-compose-mode . corfu-mode)
    ;; :load-path "/usr/local/share/emacs/site-lisp/mu4e"
    :config
    (require 'mu4e-org)
    ;; This is set to 't' to avoid mail syncing issues when using mbsync
    (setq mu4e-change-filenames-when-moving t)

    ;; Refresh mail using isync every 10 minutes
    (setq mu4e-update-interval (* 10 60))
    (setq mu4e-get-mail-command "mbsync -a")
    (setq mu4e-maildir "~/Mail")
    (setq message-send-mail-function 'smtpmail-send-it)
    (setq mu4e-compose-format-flowed t)
    (setq mu4e-compose-signature
          (concat "Philippe Gabriel - \n[[mailto:philippe.gabriel.1@umontreal.ca][philippe.gabriel.1@umontreal.ca]] | "
                  "[[mailto:pgabriel999@hotmail.com][pgabriel999@hotmail.com]]"))
    (setq mu4e-compose-signature-auto-include nil)

    (setq mu4e-contexts
          (list
           ;; Main account
           (make-mu4e-context
            :name "Main"
            :match-func
            (lambda (msg)
              (when msg
                (string-prefix-p "/Main" (mu4e-message-field msg :maildir))))
            :vars '((user-mail-address . "pgabriel999@hotmail.com")
                    (user-full-name . "Philippe Gabriel")
                    (smtpmail-smtp-server . "smtp.office365.com")
                    (smtpmail-smtp-user . "pgabriel999@hotmail.com")
                    (smtpmail-smtp-service . 587)
                    (smtpmail-stream-type . starttls)
                    (mu4e-drafts-folder . "/Main/Drafts")
                    (mu4e-sent-folder . "/Main/Sent")
                    (mu4e-refile-folder . "/Main/Archive")
                    (mu4e-trash-folder . "/Main/Deleted")))

           ;; University account
           (make-mu4e-context
            :name "University"
            :match-func
            (lambda (msg)
              (when msg
                (string-prefix-p "/University" (mu4e-message-field msg :maildir))))
            :vars '((user-mail-address . "philippe.gabriel.1@umontreal.ca")
                    (user-full-name . "Philippe Gabriel")
                    (smtpmail-smtp-server . "smtp.office365.com")
                    (smtpmail-smtp-user . "philippe.gabriel.1@umontreal.ca")
                    (smtpmail-smtp-service . 587)
                    (smtpmail-stream-type . starttls)
                    (mu4e-drafts-folder . "/University/Drafts")
                    (mu4e-sent-folder . "/University/Sent Items")
                    (mu4e-refile-folder . "/University/Archive")
                    (mu4e-trash-folder . "/University/Deleted Items")))))

    (setq mu4e-maildir-shortcuts
          '(("/University/Inbox" . ?u)
            ("/University/Drafts" . ?d)
            ("/Main/Inbox" . ?m)
            ("/Main/Jobs" . ?j)
            ("/Main/University" . ?s)))
    (mu4e t)
    :custom
    (mu4e-context-policy 'pick-first)
    ;; (mu4e-mu-binary (expand-file-name "mu/mu" (straight--repos-dir "mu")))
    ;; (setq mu4e-bookmarks
    ;;       '((:name "Display Name" :query "Query" :key "Key" ...)))
    ))

(unless pg/is-termux
  (use-package mu4e-alert
    :straight t
    :after mu4e
    :custom
    (mu4e-alert-notify-repeated-mails t)
    :config
    (mu4e-alert-set-default-style 'notifications)
    (mu4e-alert-enable-notifications)
    (mu4e-alert-enable-mode-line-display)))

(provide 'pg-email)
