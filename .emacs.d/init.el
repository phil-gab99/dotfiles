;;; init.el -*- lexical-binding: t; -*-
;; Author: Philippe Gabriel

(setq gc-cons-threshold (* 50 1000 1000)) ;; Sets garbage collection threshold high enough

(push (expand-file-name "lisp" user-emacs-directory) load-path)
(push (expand-file-name "themes" user-emacs-directory) custom-theme-load-path)

;; Fonts used
(defconst pg/font-fixed
  "JetBrains Mono"
  "Fixed pitch font.")
(defconst pg/font-variable
  "Iosevka Aile"
  "Variable pitch font.")

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
  "Sets the value of custom variables using `customize-set-variable'.
    The CUSTOM-SETS argument represents a plist where each entry's key is the
    custom variable one wishes to set and the corresponding value is the value to
    set to the custom variable."
  (mapcar (lambda (setting)
            (let ((custom (car setting))
                  (value (cdr setting)))
              (customize-set-variable custom value)))
          custom-sets))

;; (global-set-key (kbd "C-x C-c") #'pg/save-buffers-kill-emacs)

;; System related constants
(defconst pg/is-termux
  (string-suffix-p "Android" (string-trim (shell-command-to-string "uname -a")))
  "Determines whether the current system is an Android based system.")
(defconst pg/is-windows (eq system-type 'windows-nt)
  "Determines whether the current system is a Windows based system.")
(defconst pg/is-linux (eq system-type 'gnu/linux)
  "Determines whether the current system is a GNU/Linux based system.")
(defconst pg/is-guix-system
  (and pg/is-linux
       (string-match-p (regexp-quote "(guix@guix)")
                       (shell-command-to-string "cat /proc/version")))
  "Determines whether the current system is a GNU/Linux based system running the
          GNU Guix distribution.")
(defconst pg/exwm-enabled
  (getenv "EXWM")
  "Determines whether the EXWM is currently running.")

(pg/customize-set-variables
 `((load-prefer-newer . t)
   (use-short-answers . t)
   (auto-save-list-file-prefix . ,(expand-file-name "tmp/auto-saves/sessions/" user-emacs-directory))
   (warning-suppress-log-types . (lsp-mode))
   (warning-suppress-types . (lsp-mode))
   (warning-minimum-level . :error)
   (help-at-pt-display-when-idle . t)))

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
                   pg-programming-json
                   ;; pg-programming-lmc
                   pg-programming-markdown
                   pg-programming-mips
                   pg-programming-prolog
                   pg-programming-python
                   pg-programming-scala
                   ;; pg-programming-smtlibv2
                   pg-programming-sql
                   pg-programming-javascript
                   pg-programming-tex
                   pg-programming-typescript
                   ;; pg-programming-vhdl
                   pg-programming-xml
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

;; This section needs to be at the end so that Emacs doesn't complain about packages not being on load path
(unless pg/is-windows
  (require 'ob-jupyter)
  (org-babel-do-load-languages ;; Loads languages to be executed by org-babel
   'org-babel-load-languages '((emacs-lisp . t)
                               (java . t)
                               (shell . t)
                               (plantuml . t)
                               (python . t)
                               (arduino . t)
                               (jupyter . t)))

  (setq org-babel-default-header-args:jupyter-python '((:async . "yes")
                                                       (:session . "py")
                                                       (:kernel . "python3"))))

(setq gc-cons-threshold (* 2 1000 1000))
