(define mpd
  (make <service>
    #:provides '(mpd)
    #:docstring "Runs `mpd'"
    #:respawn? #f
    #:start (make-forkexec-constructor '("mpd")
                                       #:pid-file "~/.config/mpd/mpd.pid"
                                       #:log-file "~/.config/mpd/mpd.log")
    #:stop (make-kill-destructor)))

(register-services mpd)
;; (start mpd)
