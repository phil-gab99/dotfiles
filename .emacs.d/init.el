(setq gc-cons-threshold (* 50 1000 1000)) ; Sets garbage collection threshold high enough

(push "~/.emacs.d/lisp" load-path)

(defun close-all-buffers ()
  "Closes all emacs buffers."
  (interactive)
  (mapc 'kill-buffer (buffer-list)))

(defun pg/save-buffers-kill-emacs ()
  "Closes all emacs buffers before exiting emacs."
  (interactive)
  (close-all-buffers)
  (save-buffers-kill-emacs))

(global-set-key (kbd "C-x C-c") #'pg/save-buffers-kill-emacs)

(setq pg/is-termux (string-suffix-p "Android" (string-trim (shell-command-to-string "uname -a")))
      pg/exwm-enabled (and (not pg/is-termux) (display-graphic-p)))

(customize-set-variable 'load-prefer-newer t)
(customize-set-variable 'use-short-answers t)

;; (byte-recompile-directory (expand-file-name "~/.emacs.d/lisp") 0)

(require 'pg-startup)

(if pg/exwm-enabled
    (require 'pg-desktop))

(dolist (package '(pg-ui
                   pg-org
                   pg-completion
                   pg-editing
                   pg-bindings
                   pg-native-compilation
                   pg-guix
                   pg-passwords
                   pg-keylog
                   pg-buffer
                   pg-email
                   pg-editing
                   pg-help
                   pg-web
                   pg-file
                   pg-shell

                   pg-project
                   pg-programming
                   ;; pg-programming-alloy
                   pg-programming-cc
                   pg-programming-commonlisp
                   pg-programming-css
                   pg-programming-docker
                   pg-programming-git
                   pg-programming-groovy
                   pg-programming-haskell
                   pg-programming-java
                   pg-programming-lmc
                   pg-programming-markdown
                   pg-programming-mips
                   pg-programming-python
                   ;; pg-programming-smtlibv2
                   pg-programming-sql
                   pg-programming-tex
                   pg-programming-typescript
                   ;; pg-programming-vhdl
                   pg-programming-yaml

                   pg-notification
                   pg-viewers
                   pg-bible
                   pg-finance
                   pg-social
                   pg-weather
                   pg-music
                   pg-games))
  (require package))

(setq gc-cons-threshold (* 2 1000 1000))
