(straight-use-package 'slack)
(unless (fboundp 'slack-start)
  (autoload #'slack-start "slack" nil t))
(with-eval-after-load 'slack
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
    ",md" 'slack-message-delete)
  (customize-set-variable 'slack-prefer-current-team t)
  (require 'emojify)
  (with-eval-after-load 'emojify
    (customize-set-variable 'slack-buffer-emojify t)))

(unless (fboundp 'sx-search)
  (autoload #'sx-search "sx" nil t))

(require 'telega)
(with-eval-after-load 'telega
  (require 'telega-alert)
  (require 'telega-dashboard)
  (telega 1)
  (customize-set-variable 'telega-alert-mode 1))

(provide 'pg-social)
