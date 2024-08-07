;;; pg-social.el -*- lexical-binding: t; -*-
;; Author: Philippe Gabriel

(dolist (mode '(erc-mode-hook
                erc-list-menu-mode-hook))
  (add-hook mode #'(lambda ()
                     (display-line-numbers-mode 0))))
(with-eval-after-load 'erc
  (unless (fboundp 'corfu-mode)
    (autoload #'corfu-mode "corfu" nil t))
  (add-hook 'erc-mode-hook #'corfu-mode)
  (setopt erc-server "irc.libera.chat"
          erc-nick (plist-get pg/user :user)
          erc-user (plist-get pg/user :name)
          erc-kill-buffer-on-part t
          erc-auto-query bury))

(defun pg/ement-connect ()
  "Connects to matrix client with username and password supplied."
  (interactive)
  (ement-connect :user-id (concat "@" (plist-get pg/user :user) ":matrix.org")
                 :password (pg/lookup-password :host "matrix.org"
                                               :user (plist-get pg/user :user))))

(straight-use-package 'ement)
(unless (fboundp 'ement-connect)
  (autoload #'ement-connect "ement" nil t))
(add-hook 'ement-room-mode-hook #'(lambda ()
                                    (display-line-numbers-mode 0)))

(with-eval-after-load 'general
  (pg/leader-keys
    "c" '(:ignore t :which-key "comms")
    "cm" '(:ignore t :which-key "matrix")
    "cmc" '(pg/ement-connect :which-key "start")))

(with-eval-after-load 'ement
  (setopt ement-room-prism 'both
          ement-room-sender-headers t)
  (with-eval-after-load 'general
    (pg/leader-keys
      "cmd" '(ement-disconnect :which-key "disconnect"))))

(straight-use-package 'slack)
(unless (fboundp 'slack-start)
  (autoload #'slack-start "slack" nil t))
(add-hook 'slack-mode-hook #'(lambda ()
                               (display-line-numbers-mode 0)))
(with-eval-after-load 'slack
  (setopt slack-prefer-current-team t
          slack-buffer-emojify t)
  (slack-register-team :name "ift6755"
                       :default t
                       :token (pg/lookup-password :host "ift6755.slack.com"
                                                  :user (plist-get pg/user :email-udem))
                       :cookie (pg/lookup-password :host "ift6755.slack.com"
                                                   :user (concat (plist-get pg/user :email-udem) "^cookie"))
                       :subscribed-channels '((general questions random))
                       :modeline-enabled t)
  (with-eval-after-load 'evil
    (evil-define-key 'normal slack-info-mode-map
      ",u" #'slack-room-update-messages)
    (evil-define-key 'normal slack-mode-map
      ",ra" #'slack-message-add-reaction
      ",rr" #'slack-message-remove-reaction
      ",rs" #'slack-message-show-reaction-users
      ",mm" #'slack-message-write-another-buffer
      ",me" #'slack-message-edit
      ",md" #'slack-message-delete))
  (with-eval-after-load 'general
    (pg/leader-keys
      "csc" '(slack-channel-select :which-key "channels")
      "csm" '(slack-im-select :which-key "message")
      "csr" '(slack-message-add-reaction :which-key "react"))))

(with-eval-after-load 'general
  (pg/leader-keys
    "c" '(:ignore t :which-key "comms")
    "cs" '(:ignore t :which-key "slack")
    "csc" '(slack-start :which-key "start")))

(straight-use-package 'sx)
(unless (fboundp 'sx-search)
  (autoload #'sx-search "sx" nil t))

(provide 'pg-social)
