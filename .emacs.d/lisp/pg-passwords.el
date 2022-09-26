;;; pg-passwords.el -*- lexical-binding: t; -*-
;; Author: Philippe Gabriel

(require 'auth-source)
(with-eval-after-load 'auth-source
  (customize-set-variable 'auth-sources '("~/.authinfo.gpg")))

(unless pg/is-termux
  (require 'epg-config)
  (with-eval-after-load 'epg-config
    (customize-set-variable 'epg-pinentry-mode 'loopback)))

(unless pg/is-termux
  (straight-use-package 'pinentry)
  (require 'pinentry)
  (with-eval-after-load 'pinentry
    (pinentry-start)))

(require 'password-cache)
(with-eval-after-load 'password-cache
  (customize-set-variable 'password-cache-expiry (* 60 60 2)))

(straight-use-package 'password-store)
(require 'password-store)
(with-eval-after-load 'password-store
  (customize-set-variable 'password-store-time-before-clipboard-restore 60)
  (with-eval-after-load 'general
    (pg/leader-keys
      "a" '(:ignore t :which-key "pass")
      "ac" '(password-store-copy :which-key "copy")
      "af" '(password-store-copy-field :which-key "copy field"))))

(defun pg/lookup-password (&rest keys)
  "Looks up passwords from `authinfo' entries."
  (let ((result (apply #'auth-source-search keys)))
    (if result
        (funcall (plist-get (car result) :secret))
      nil)))

(provide 'pg-passwords)
