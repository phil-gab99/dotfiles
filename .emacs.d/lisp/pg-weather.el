;;; pg-weather.el -*- lexical-binding: t; -*-
;; Author: Philippe Gabriel

(defun pg/wttrin-fetch-raw-string (query)
  "Get the weather information based on your QUERY."
  (let ((url-user-agent "curl"))
    (add-to-list 'url-request-extra-headers wttrin-default-accept-language)
    (with-current-buffer
        (url-retrieve-synchronously
         (concat "http://wttr.in/" query)
         (lambda (status) (switch-to-buffer (current-buffer))))
      (decode-coding-string (buffer-string) 'utf-8))))

(straight-use-package 'wttrin)
(unless (fboundp 'wttrin)
  (autoload #'wttrin "wttrin" nil t))
(with-eval-after-load 'wttrin
  (fset #'wttrin-fetch-raw-string #'pg/wttrin-fetch-raw-string)
  (setopt wttrin-default-cities (list (plist-get pg/user :city))
          wttrin-default-accept-language '("Accept-Language" . "en-US")))

(provide 'pg-weather)
