(require 'pg-startup)

(use-package auth-source
  :straight nil
  :custom
  (auth-sources '("~/.authinfo.gpg")))

(unless pg/is-termux
  (use-package pinentry
    :straight t
    :custom
    (epg-pinentry-mode 'loopback)
    :config
    (pinentry-start)))

(use-package password-cache
  :straight nil
  :custom
  (password-cache-expiry (* 60 60 2)))

(use-package password-store
  :straight t)

(defun pg/lookup-password (&rest keys)
  (let ((result (apply #'auth-source-search keys)))
    (if result
        (funcall (plist-get (car result) :secret))
      nil)))

(provide 'pg-passwords)
