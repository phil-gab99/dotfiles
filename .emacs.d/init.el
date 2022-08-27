(setq gc-cons-threshold (* 50 1000 1000)) ; Sets garbage collection threshold high enough

(add-to-list 'load-path "~/.emacs.d/lisp")

(setq pg/is-termux (string-suffix-p "Android" (string-trim (shell-command-to-string "uname -a")))
      pg/exwm-enabled (and (not pg/is-termux) (display-graphic-p)))

(dolist (package '(pg-startup
                   ;; pg-native-compilation
                   pg-ui
                   pg-guix
                   pg-passwords
                   pg-keylog
                   pg-completion
                   pg-buffer
                   pg-email
                   pg-editing
                   pg-help
                   pg-web
                   pg-file
                   pg-shell

                   pg-project
                   pg-programming
                   pg-programming-alloy
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
                   pg-programming-smtlibv2
                   pg-programming-sql
                   pg-programming-tex
                   pg-programming-typescript
                   pg-programming-vhdl
                   pg-programming-yaml

                   pg-notification
                   pg-org
                   pg-viewers
                   ;; pg-bible
                   pg-finance
                   pg-social
                   pg-weather
                   pg-music
                   pg-games
                   pg-bindings))
  (require package))

(when pg/exwm-enabled (require 'pg-desktop))

(setq gc-cons-threshold (* 2 1000 1000))
