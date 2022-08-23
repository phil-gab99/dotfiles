(use-package slack
  :straight t
  :commands slack-start
  :hook (slack-mode . corfu-mode)
  :config
  (slack-register-team :name "ift6755"
                       :default t
                       :token (pg/lookup-password
                               :host "ift6755.slack.com"
                               :user "philippe.gabriel.1@umontreal.ca")
                       :cookie (pg/lookup-password
                                :host "ift6755.slack.com"
                                :user "philippe.gabriel.1@umontreal.ca^cookie")
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
    ",md" 'slack-message-delete)
  :custom
  (slack-buffer-emojify t)
  (slack-prefer-current-team t))

(use-package sx
  :straight t
  :commands sx-search)

(require 'telega-alert)
(require 'telega-dashboard)
(use-package telega
  :straight nil
  :init (telega 1)
  :config
  (telega-alert-mode 1))

(provide 'pg-social)
