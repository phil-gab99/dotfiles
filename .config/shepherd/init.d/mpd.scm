;; (define mpd
;;   (make <service>
;;     #:provides '(mpd)
;;     #:docstring "Runs `mpd'"
;;     #:respawn? #f
;;     #:start (make-forkexec-constructor '("mpd")
;;                                        #:log-file "~/.config/mpd/mpd.log")
;;     #:stop (make-kill-destructor)))

;; (register-services mpd)
