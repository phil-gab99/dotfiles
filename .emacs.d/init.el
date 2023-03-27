;;; init.el -*- lexical-binding: t; -*-
;; Author: Philippe Gabriel

(setq gc-cons-threshold (* 50 1000 1000)) ;; Sets garbage collection threshold high enough

(push "~/.emacs.d/lisp" load-path)
(push "~/.emacs.d/themes" custom-theme-load-path)

(defun pg/close-all-buffers ()
  "Closes all emacs buffers."
  (interactive)
  (mapc 'kill-buffer (buffer-list)))

(defun pg/save-buffers-kill-emacs ()
  "Closes all emacs buffers before exiting emacs."
  (interactive)
  (pg/close-all-buffers)
  (save-buffers-kill-emacs))

(defun pg/customize-set-variables (custom-sets)
  "Sets the value of custom variables using `customize-set-variable'. The
CUSTOM-SETS argument represents a plist where each entry's key is the custom
variable one wishes to set and the corresponding value is the value to set to
the custom variable."
  (mapcar (lambda (setting)
            (let ((custom (car setting))
                  (value (cdr setting)))
              (customize-set-variable custom value)))
          custom-sets))

;; (global-set-key (kbd "C-x C-c") #'pg/save-buffers-kill-emacs)

(setq pg/is-termux (string-suffix-p "Android" (string-trim (shell-command-to-string "uname -a")))
      pg/is-windows (eq system-type 'windows-nt)
      pg/is-linux (eq system-type 'gnu/linux)
      pg/is-guix-system (and pg/is-linux
                             (string-match-p (regexp-quote "(guix@guix)")
                                             (shell-command-to-string "cat /proc/version")))
      pg/exwm-enabled (and (not pg/is-termux)
                           (display-graphic-p)
                           pg/is-linux))

(pg/customize-set-variables
 `((load-prefer-newer . t)
   (use-short-answers . t)
   (auto-save-list-file-prefix . ,(expand-file-name "tmp/auto-saves/sessions/" user-emacs-directory))))

;; (byte-recompile-directory (expand-file-name "~/.emacs.d/lisp") 0)

(require 'pg-startup)

(if pg/exwm-enabled (require 'pg-desktop))

(dolist (package '(pg-ui
                   pg-bindings
                   pg-org
                   pg-completion
                   pg-editing
                   pg-native-compilation
                   pg-guix
                   pg-passwords
                   pg-keylog
                   pg-buffer
                   pg-email
                   pg-help
                   pg-web
                   pg-file
                   pg-shell

                   pg-project
                   pg-programming
                   ;; pg-programming-alloy
                   pg-programming-arduino
                   pg-programming-cc
                   ;; pg-programming-commonlisp
                   pg-programming-css
                   pg-programming-docker
                   pg-programming-elisp
                   pg-programming-git
                   pg-programming-groovy
                   pg-programming-haskell
                   pg-programming-html
                   pg-programming-java
                   ;; pg-programming-lmc
                   pg-programming-markdown
                   pg-programming-mips
                   pg-programming-prolog
                   pg-programming-python
                   ;; pg-programming-smtlibv2
                   pg-programming-sql
                   pg-programming-tex
                   pg-programming-typescript
                   ;; pg-programming-vhdl
                   pg-programming-yaml

                   pg-notification
                   pg-viewers
                   ;; pg-bible
                   pg-finance
                   pg-social
                   pg-weather
                   pg-music
                   pg-games))
  (require package))

(setq gc-cons-threshold (* 2 1000 1000))
