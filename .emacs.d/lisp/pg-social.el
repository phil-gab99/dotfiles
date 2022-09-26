;;; pg-social.el -*- lexical-binding: t; -*-
;; Author: Philippe Gabriel

(straight-use-package 'slack)
(unless (fboundp 'slack-start)
  (autoload #'slack-start "slack" nil t))
(with-eval-after-load 'slack
  (pg/customize-set-variables
   '((slack-prefer-current-team . t)
     (slack-buffer-emojify . t)))
  (slack-register-team :name "ift6755"
                       :default t
                       :token (pg/lookup-password :host "ift6755.slack.com" :user "philippe.gabriel.1@umontreal.ca")
                       :cookie (pg/lookup-password :host "ift6755.slack.com" :user "philippe.gabriel.1@umontreal.ca^cookie")
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
      "c" '(:ignore t :which-key "slack")
      "cs" '(slack-start :which-key "start")
      "cc" '(slack-channel-select :which-key "channels")
      "cm" '(slack-im-select :which-key "message")
      "cr" '(slack-message-add-reaction :which-key "react"))))

(straight-use-package 'sx)
(unless (fboundp 'sx-search)
  (autoload #'sx-search "sx" nil t))

(unless (fboundp 'telega)
  (autoload #'telega "telega" nil t))
(with-eval-after-load 'telega
  (require 'telega-alert)
  (require 'telega-dashboard)
  (customize-set-variable 'telega-alert-mode 1))

(provide 'pg-social)
