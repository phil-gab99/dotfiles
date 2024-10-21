;;; pg-email.el -*- lexical-binding: t; -*-
;; Author: Philippe Gabriel

(unless (or pg/is-termux
            pg/is-windows)
  (unless (fboundp 'mu4e)
    (autoload #'mu4e "mu4e" nil t))
  (unless (fboundp 'mu4e-compose-new)
    (autoload #'mu4e-compose-new "mu4e" nil t))
  (unless (fboundp 'corfu-mode)
    (autoload #'corfu-mode "corfu" nil t))
  (add-hook 'mu4e-main-mode-hook #'(lambda ()
                                     (display-line-numbers-mode 0)))
  (add-hook 'mu4e-view-mode-hook #'(lambda ()
                                     (display-line-numbers-mode 0)))
  (add-hook 'mu4e-compose-mode-hook #'corfu-mode)
  (remove-hook 'mu4e-main-mode-hook 'evil-collection-mu4e-update-main-view)
  (with-eval-after-load 'general
    (pg/leader-keys
      "m" '(:ignore t :which-key "email")
      "md" '(mu4e :which-key "dashboard")
      "mc" '(mu4e-compose-new :which-key "compose")))
  (with-eval-after-load 'mu4e
    (require 'mu4e-org)
    (setopt mail-user-agent #'mu4e-user-agent
            mu4e-change-filenames-when-moving t
            mu4e-update-interval (* 10 60)
            mu4e-get-mail-command "mbsync -a"
            mu4e-compose-format-flowed t
            mu4e-compose-signature-auto-include nil
            mu4e-sent-messages-behavior 'delete
            message-send-mail-function #'smtpmail-send-it
            mu4e-attachment-dir (plist-get pg/user :download)
            mu4e-maildir-shortcuts '(("/Main/Inbox" . 109))
            mu4e-context-policy 'pick-first)
    (add-to-list 'mu4e-bookmarks
                 '( :name "Starred"
                    :query "flag:flagged"
                    :key ?f))
    (with-eval-after-load 'org-contacts
      (add-to-list 'mu4e-headers-actions
                   '("org-contact-add" . mu4e-action-add-org-contact) t)
      (add-to-list 'mu4e-view-actions
                   '("org-contact-add" . mu4e-action-add-org-contact) t))

    (setq mu4e-contexts
          (list
           (make-mu4e-context :name "Main"
                              :match-func (lambda (msg)
                                            (when msg
                                              (string-prefix-p "/Main" (mu4e-message-field msg :maildir))))
                              :vars `((user-mail-address . ,(plist-get pg/user :email))
                                      (user-full-name . ,(plist-get pg/user :name))
                                      (smtpmail-smtp-server . "smtp.gmail.com")
                                      (smtpmail-auth-credentials (expand-file-name "~/.authinfo.gpg"))
                                      (smtpmail-smtp-user . ,(plist-get pg/user :email))
                                      (smtpmail-smtp-service . 587)
                                      (smtpmail-stream-type . starttls)
                                      (mu4e-drafts-folder . "/Main/Drafts")
                                      (mu4e-sent-folder . "/Main/Sent")
                                      (mu4e-refile-folder . "/Main/Archive")
                                      (mu4e-trash-folder . "/Main/Trash")))))))

(unless (or pg/is-termux
            pg/is-windows)
  (straight-use-package 'mu4e-alert)
  (with-eval-after-load 'mu4e
    (require 'mu4e-alert))
  (with-eval-after-load 'mu4e-alert
    (setopt mu4e-alert-notify-repeated-mails t
            mu4e-alert-email-notification-types '(subjects))
    (mu4e-alert-set-default-style 'notifications)
    (mu4e-alert-enable-notifications)
    (mu4e-alert-enable-mode-line-display)))

(provide 'pg-email)
