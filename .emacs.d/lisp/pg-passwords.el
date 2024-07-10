;;; pg-passwords.el -*- lexical-binding: t; -*-
;; Author: Philippe Gabriel

(when pg/is-linux
  (require 'auth-source)
  (with-eval-after-load 'auth-source
    (customize-set-variable 'auth-sources '("~/.authinfo.gpg"))))

(unless (or pg/is-termux
            pg/is-windows)
  (require 'epg-config)
  (with-eval-after-load 'epg-config
    (customize-set-variable 'epg-pinentry-mode 'loopback)))

(unless (or pg/is-termux
            pg/is-windows)
  (require 'pinentry)
  (with-eval-after-load 'pinentry
    (pinentry-start)))

(require 'password-cache)
(with-eval-after-load 'password-cache
  (customize-set-variable 'password-cache-expiry (* 60 60 2)))

(unless pg/is-windows
  (straight-use-package 'password-store)
  (unless (fboundp 'password-store-copy)
    (autoload #'password-store-copy "password-store" nil t))
  (unless (fboundp 'password-store-copy-field)
    (autoload #'password-store-copy-field "password-store" nil t)))
  (with-eval-after-load 'general
    (pg/leader-keys
      "a" '(:ignore t :which-key "pass")
      "ac" '(password-store-copy :which-key "copy")
      "af" '(password-store-copy-field :which-key "copy field")))
  (with-eval-after-load 'password-store
    (customize-set-variable 'password-store-time-before-clipboard-restore 60))

(defun pg/lookup-password (&rest keys)
  "Looks up passwords from authinfo entries."
  (let ((result (apply #'auth-source-search keys)))
    (if result
        (funcall (plist-get (car result) :secret))
      nil)))

(provide 'pg-passwords)
