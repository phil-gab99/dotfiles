;;; pg-startup.el -*- lexical-binding: t; -*-
;; Author: Philippe Gabriel

(pg/set-global-key "C-x C-c" pg/save-buffers-kill-emacs)
(pg/set-custom ((gc-cons-threshold . (* 50 1000 1000))
                (load-prefer-newer . t)
                (use-short-answers . t)
                (auto-save-list-file-prefix . (expand-file-name "tmp/auto-saves/sessions/" user-emacs-directory))
                (warning-suppress-log-types . '((lsp-mode)))
                (warning-suppress-types . '((lsp-mode)))
                (warning-minimum-level . :error)
                (help-at-pt-display-when-idle . t)))

(when (featurep 'native-compile)
  (pg/set-custom native-comp-async-report-warnings-errors nil) ;; Silence compiler warnings
  (add-to-list 'native-comp-eln-load-path (expand-file-name "eln-cache/" user-emacs-directory))) ;; Set directory for cache storage

;; Eventually try elpaca over straight for non-guix systems https://github.com/progfolio/elpaca
(unless (and (featurep 'straight) ;; pg/is-guix-system TODO: Move to guix packages
             )
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

(provide 'pg-startup)
