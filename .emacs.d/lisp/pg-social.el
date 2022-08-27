(use-package slack
  :straight t
  :init (require 'emojify)
  :after evil
  :commands slack-start
  :custom
  (slack-prefer-current-team t)
  (slack-buffer-emojify t)
  :config
  (slack-register-team :name "ift6755"
                       :default t
                       :token (pg/lookup-password :host "ift6755.slack.com" :user "philippe.gabriel.1@umontreal.ca")
                       :cookie (pg/lookup-password :host "ift6755.slack.com" :user "philippe.gabriel.1@umontreal.ca^cookie")
                       :subscribed-channels '((general questions random))
                       :modeline-enabled t)

  (evil-define-key 'normal slack-info-mode-map
    ",u" 'slack-room-update-messages)
  (evil-define-key 'normal slack-mode-map
    ",ra" 'slack-message-add-reaction
    ",rr" 'slack-message-remove-reaction
    ",rs" 'slack-message-show-reaction-users
    ",mm" 'slack-message-write-another-buffer
    ",me" 'slack-message-edit
    ",md" 'slack-message-delete))

(use-package sx
  :straight t
  :commands sx-search)

(use-package telega
  :straight nil
  :init
  (require 'telega-alert)
  (require 'telega-dashboard)
  :custom
  (telega-alert-mode 1)
  :config
  (telega 1))

(provide 'pg-social)
