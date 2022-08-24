(require 'auth-source)
(with-eval-after-load 'auth-source
  (customize-set-variable 'auth-sources '("~/.authinfo.gpg")))

(unless pg/is-termux
  (require 'epg-config)
  (require 'pinentry)
  (with-eval-after-load 'epg-config
    (customize-set-variable 'epg-pinentry-mode 'loopback)
    (pinentry-start)))

(require 'password-cache)
(with-eval-after-load 'password-cache
  (customize-set-variable 'password-cache-expiry (* 60 60 2)))

(require 'password-store)
(with-eval-after-load 'password-store
  (customize-set-variable 'password-store-time-before-clipboard-restore 60))

(defun pg/lookup-password (&rest keys)
  "Looks up passwords from `authinfo' entries."
  (let ((result (apply #'auth-source-search keys)))
    (if result
        (funcall (plist-get (car result) :secret))
      nil)))

(provide 'pg-passwords)
