(define mpd
  (make <service>
    #:provides '(mpd)
    #:docstring "Runs `mpd'"
    #:respawn? #t
    #:start (make-system-constructor "mpd")
    #:stop (make-system-destructor "mpd --kill")))

(register-services mpd)
;; (start mpd)
