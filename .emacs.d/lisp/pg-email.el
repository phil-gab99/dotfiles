(unless pg/is-termux
  (straight-use-package '(mu :type git
                             :host github
                             :repo "djcb/mu"
                             :branch "release/1.8"))
  (require 'mu4e (expand-file-name "straight/repos/mu/mu4e/mu4e.el" user-emacs-directory))
  (with-eval-after-load 'mu4e
    (require 'mu4e-org (expand-file-name "straight/repos/mu/mu4e/mu4e-org.el" user-emacs-directory))
    (add-hook 'mu4e-compose-mode-hook #'corfu-mode)
    (customize-set-variable 'mail-user-agent #'mu4e-user-agent)
    (customize-set-variable 'mu4e-change-filenames-when-moving t)
    (customize-set-variable 'mu4e-update-interval (* 10 60))
    (customize-set-variable 'mu4e-get-mail-command "mbsync -a")
    ;; (customize-set-variable 'mu4e-maildir "~/Mail")
    (customize-set-variable 'mu4e-compose-format-flowed t)
    (customize-set-variable 'mu4e-compose-signature
                            (concat "Philippe Gabriel - \n[[mailto:philippe.gabriel.1@umontreal.ca][philippe.gabriel.1@umontreal.ca]] | "
                                    "[[mailto:pgabriel999@hotmail.com][pgabriel999@hotmail.com]]"))
    (customize-set-variable 'mu4e-compose-signature-auto-include nil)
    (customize-set-variable 'message-send-mail-function 'smtpmail-send-it)
    (customize-set-variable 'mu4e-maildir-shortcuts
                            '(("/University/Inbox" . ?u)
                              ("/University/Drafts" . ?d)
                              ("/Main/Inbox" . ?m)
                              ("/Main/Jobs" . ?j)
                              ("/Main/University" . ?s)))
    (customize-set-variable 'mu4e-context-policy 'pick-first)

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
                    (mu4e-trash-folder . "/University/Deleted Items")))))))

(straight-use-package 'mu4e-alert)
(unless pg/is-termux
  (require 'mu4e-alert)
  (with-eval-after-load 'mu4e
    (customize-set-variable 'mu4e-alert-notify-repeated-mails t)
    (mu4e-alert-set-default-style 'notifications)
    (mu4e-alert-enable-notifications)
    (mu4e-alert-enable-mode-line-display)))

(provide 'pg-email)
