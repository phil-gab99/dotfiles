(define mpd
  (make <service>
    #:provides '(mpd)
    #:docstring "Runs `mpd'"
    #:respawn? #f
    #:start (make-forkexec-constructor '("mpd")
                                       #:pid-file "/home/phil-gab99/.config/mpd.pid")
    #:stop (make-kill-destructor)))

(register-services mpd)
;; (start mpd)
