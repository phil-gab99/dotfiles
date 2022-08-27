(use-package auth-source
  :straight nil
  :custom
  (auth-sources '("~/.authinfo.gpg")))

(unless pg/is-termux
  (use-package epg-config
    :straight nil
    :custom
    (epg-pinentry-mode 'loopback)))

(unless pg/is-termux
  (use-package pinentry
    :straight t
    :config
    (pinentry-start)))

(use-package password-cache
  :straight nil
  :custom
  (password-cache-expiry (* 60 60 2)))

(use-package password-store
  :straight t
  :custom
  (password-store-time-before-clipboard-restore 60))

(defun pg/lookup-password (&rest keys)
  "Looks up passwords from `authinfo' entries."
  (let ((result (apply #'auth-source-search keys)))
    (if result
        (funcall (plist-get (car result) :secret))
      nil)))

(provide 'pg-passwords)
