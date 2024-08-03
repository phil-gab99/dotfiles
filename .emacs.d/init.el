;;; init.el -*- lexical-binding: t; -*-
;; Author: Philippe Gabriel

(push (expand-file-name "lisp" user-emacs-directory) load-path)
(push (expand-file-name "themes" user-emacs-directory) custom-theme-load-path)

(defconst pg/user
  `( :user "phil-gab99"
     :name "Philippe Gabriel"
     :email "pgabriel999@hotmail.com"
     :email-udem "philippe.gabriel.1@umontreal.ca"
     :city "montreal"
     :font-fixed "JetBrains Mono"
     :font-variable "Iosevka Aile"
     :home ,(or (getenv "HOME") (expand-file-name "~"))
     :cache ,(or (getenv "XDG_CACHE_HOME") (expand-file-name ".cache" "~"))
     :config ,(or (getenv "XDG_CONFIG_HOME") (expand-file-name ".config" "~"))
     :documents ,(or (getenv "XDG_DOCUMENTS_DIR") (expand-file-name "Documents" "~"))
     :download ,(or (getenv "XDG_DOWNLOAD_DIR") (expand-file-name "Downloads" "~"))
     :music ,(or (getenv "XDG_MUSIC_DIR") (expand-file-name "Music" "~"))
     :dotfiles ,(expand-file-name ".dotfiles" "~")
     :guix-home-profile ,(expand-file-name ".guix-home/profile" "~"))
  "Plist holding user details.")

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

(defun pg/close-all-buffers ()
  "Closes all emacs buffers."
  (interactive)
  (mapc 'kill-buffer (buffer-list)))

(defun pg/save-buffers-kill-emacs ()
  "Closes all emacs buffers before exiting emacs."
  (interactive)
  (if pg/exwm-enabled (pg/kill-panel))
  (save-buffers-kill-emacs))

(defmacro pg/set-global-key (binding command)
  "Set binding to global keymap with respect to emacs version."
  (if (version< emacs-version "29")
      `(global-set-key (kbd ,binding) #',command)
    `(keymap-global-set ,binding #',command)))

(defmacro pg/set-custom (pairs)
  "Sets custom variables with respect to value depending on emacs version.

PAIRS is an alist of custom variables mapped to their desired value."
  (if (not (version< emacs-version "29"))
      (flatten-list `(setopt ,pairs))
    (macroexp-progn
     (mapcar (lambda (pair)
               `(customize-set-variable ',(car pair) ,(cdr pair)))
             pairs))))

(cond (pg/is-termux  (require 'pg-termux-config))
      (pg/is-windows (require 'pg-windows-config))
      (pg/is-guix-system (require 'pg-guix-config))
      (pg/is-linux-system (require 'pg-linux-config)))

(require 'pg-startup)
(if pg/exwm-enabled (require 'pg-desktop))

(dolist (package '(pg-ui
                   pg-bindings
                   pg-org
                   pg-completion
                   pg-editing
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
                   pg-programming-ada
                   ;; pg-programming-alloy
                   pg-programming-arduino
                   pg-programming-cc
                   pg-programming-clojure
                   ;; pg-programming-commonlisp
                   pg-programming-css
                   pg-programming-docker
                   pg-programming-elisp
                   pg-programming-erlang
                   pg-programming-gdscript
                   pg-programming-git
                   pg-programming-groovy
                   pg-programming-haskell
                   pg-programming-html
                   pg-programming-i3config
                   pg-programming-java
                   pg-programming-json
                   ;; pg-programming-lmc
                   pg-programming-markdown
                   pg-programming-mips
                   pg-programming-php
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
  (with-eval-after-load 'jupyter
    (require 'ob-jupyter))
  (org-babel-do-load-languages ;; Loads languages to be executed by org-babel
   'org-babel-load-languages '((emacs-lisp . t)
                               (java . t)
                               (shell . t)
                               (python . t)
                               (arduino . t)
                               (jupyter . t)))

  (setq org-babel-default-header-args:jupyter-python '((:async . "yes")
                                                       (:session . "py")
                                                       (:kernel . "python3")))
  (setf (alist-get "jupyter-python" org-src-lang-modes nil nil #'equal) 'python-ts))

(setq gc-cons-threshold (* 2 1000 1000))
