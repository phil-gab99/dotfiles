;;; pg-startup.el -*- lexical-binding: t; -*-
;; Author: Philippe Gabriel

(server-start)

(unless (featurep 'straight)
  (defvar bootstrap-version)
  (let ((bootstrap-file
         (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
        (bootstrap-version 5))
    (unless (file-exists-p bootstrap-file)
      (with-current-buffer
          (url-retrieve-synchronously
           "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
           'silent 'inhibit-cookies)
        (goto-char (point-max))
        (eval-print-last-sexp)))
    (load bootstrap-file nil 'nomessage)))

(defun pg/customize-set-variables (custom-sets)
  "Sets the default value of variables. The `custom-sets' argument represents
  a plist where each entry's key is the custom variable one wishes to set and
  the corresponding value is the value to set to the custom variable."
  (if (mapcar (lambda (setting)
                (let ((custom (car setting))
                      (value (cdr setting)))
                  (customize-set-variable custom value)))
              custom-sets)
      t
    nil))

(straight-use-package 'use-package)
(require 'use-package)

(provide 'pg-startup)
