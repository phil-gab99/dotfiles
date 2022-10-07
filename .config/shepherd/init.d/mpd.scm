(define mpd
  (make <service>
    #:provides '(mpd)
    #:docstring "Runs `mpd'"
    #:respawn? #t
    #:start (make-forkexec-constructor '("mpd"))
    #:stop (make-kill-destructor)))

(register-services mpd)
;; (start mpd)
